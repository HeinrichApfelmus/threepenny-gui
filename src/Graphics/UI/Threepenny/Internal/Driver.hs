{-# LANGUAGE CPP, PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Graphics.UI.Threepenny.Internal.Driver
  (
  -- * Synopsis
  -- | The main internal functionality.
      
  -- * Server running
   serve
  ,loadFile
  ,loadDirectory

  -- * Elements
  ,newElement
  ,appendElementTo
  ,emptyEl
  ,delete
  
  -- * Event handling
  -- $eventhandling
  ,bind
  ,disconnect
  ,module Reactive.Threepenny
 
  -- * Querying
  -- $querying
  ,getHead
  ,getBody
  ,getElementsByTagName
  ,getElementsById
  ,getElementsByClassName
  ,getWindow
  ,getValuesList
  ,getRequestCookies
  ,getRequestLocation
  
  -- * Utilities
  ,debug
  ,callDeferredFunction
  ,atomic

  -- * JavaScript FFI
  ,ToJS, FFI, ffi, JSFunction
  ,runFunction, callFunction
  
  -- * Types
  ,Window
  ,Element
  ,Config(..)
  ,EventData(..)
  ) where



import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Chan.Extra
import           Control.Concurrent.Delay
import           Control.DeepSeq
import qualified Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified "MonadCatchIO-transformers" Control.Monad.CatchIO as E
import           Data.ByteString               (ByteString)
import           Data.ByteString.UTF8          (toString,fromString)
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Text                     (Text,pack,unpack)
import qualified Data.Text                     as Text
import           Data.Text.Encoding            as Text
import           Data.Time
import           Network.URI
import qualified Network.WebSockets            as WS
import qualified Network.WebSockets.Snap       as WS
import qualified Data.Attoparsec.Enumerator    as Atto
import           Prelude                       hiding (init)
import           Safe
import           Snap.Core
import qualified Snap.Http.Server              as Snap
import           Snap.Util.FileServe
import           System.Environment            (getEnvironment)
import           System.FilePath

import qualified Data.Aeson                    as JSON
import           Data.Aeson                    (Result(..))
import           Data.Aeson.Generic
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Data

import           Graphics.UI.Threepenny.Internal.Types     as Threepenny
import           Graphics.UI.Threepenny.Internal.Resources
import           Graphics.UI.Threepenny.Internal.FFI
import           Reactive.Threepenny

import qualified Foreign.Coupon as Foreign
import qualified System.Mem

{-----------------------------------------------------------------------------
    Server and and session management
------------------------------------------------------------------------------}
newServerState :: IO ServerState
newServerState = ServerState 
    <$> newMVar M.empty
    <*> newMVar (0,M.empty)
    <*> newMVar (0,M.empty)

-- | Run a TP server with Snap on the specified port and the given
--   worker action.
serve :: Config -> (Session -> IO ()) -> IO ()
serve Config{..} worker = do
    env    <- getEnvironment
    let portEnv = Safe.readMay =<< Prelude.lookup "PORT" env
    
    server <- newServerState
    _      <- forkIO $ custodian 30 (sSessions server)
    let config = Snap.setPort      (maybe defaultPort id (tpPort `mplus` portEnv))
               $ Snap.setErrorLog  (Snap.ConfigIoLog tpLog)
               $ Snap.setAccessLog (Snap.ConfigIoLog tpLog)
               $ Snap.defaultConfig
    Snap.httpServe config . route $
        routeResources tpCustomHTML tpStatic server
        -- ++ routeCommunication worker server
        ++ routeWebsockets worker server

-- | Kill sessions after at least n seconds of disconnectedness.
custodian :: Integer -> MVar Sessions -> IO ()
custodian seconds sessions = forever $ do
  delaySeconds seconds
  modifyMVar_ sessions $ \sessions -> do
    killed <- fmap catMaybes $ forM (M.assocs sessions) $ \(key,Session{..}) -> do
      state <- readMVar sConnectedState
      case state of
        Connected -> return Nothing
        Disconnected time -> do
          now <- getCurrentTime
          let dcSeconds = diffUTCTime now time
          -- session is disconnected for more than  seconds
          if (dcSeconds > fromIntegral seconds)
             then do killThread sThreadId
                     return (Just key)
             else return Nothing
    
    -- remove killed sessions from the map
    return (M.filterWithKey (\k _ -> not (k `elem` killed)) sessions)

-- Run a snap action with the given session.
withSession :: ServerState -> (Session -> Snap a) -> Snap a
withSession server cont = do
    token <- readInput "token"
    case token of
        Nothing    -> error $ "Invalid session token format."
        Just token -> withGivenSession token server cont

-- Do something with the session given by its token id.
withGivenSession :: Integer -> ServerState -> (Session -> Snap a) -> Snap a
withGivenSession token ServerState{..} cont = do
    sessions <- liftIO $ withMVar sSessions return
    case M.lookup token sessions of
        Nothing      -> error $ "Nonexistant token: " ++ show token
        Just session -> cont session

{-----------------------------------------------------------------------------
    Implementation of two-way communication
    - POST and GET requests
------------------------------------------------------------------------------}
-- | Route the communication between JavaScript and the server
routeCommunication :: (Session -> IO a) -> ServerState -> Routes
routeCommunication worker server =
    [("/init"   , init worker server)
    ,("/poll"   , withSession server poll  )
    ,("/signal" , withSession server signal)
    ]

-- | Make a new session.
newSession :: ServerState -> (URI,[(String, String)]) -> Integer -> IO Session
newSession sServerState sStartInfo sToken = do
    sSignals          <- newChan
    sInstructions     <- newChan
    sMutex            <- newMVar ()
    sEventQuit        <- newEvent
    sPrizeBooth      <- Foreign.newPrizeBooth
    let sHeadElement  =  undefined -- filled in later
    let sBodyElement  =  undefined
    now               <- getCurrentTime
    sConnectedState   <- newMVar (Disconnected now)
    sThreadId         <- myThreadId
    sClosures         <- newMVar [0..]
    let session = Session {..}
    initializeElements session    

-- | Make a new session and add it to the server
createSession :: (Session -> IO void) -> ServerState -> Snap Session
createSession worker server = do
    -- uri    <- snapRequestURI
    let uri = undefined -- FIXME: No URI for WebSocket requests.
    params <- snapRequestCookies
    liftIO $ modifyMVar (sSessions server) $ \sessions -> do
        let newKey = maybe 0 (+1) (lastMay (M.keys sessions))
        session <- newSession server (uri,params) newKey
        _       <- forkIO $ void $ worker session >> handleEvents session
        return (M.insert newKey session sessions, session)

-- | Respond to initialization request.
init :: (Session -> IO void) -> ServerState -> Snap ()
init worker server = do
    session <- createSession worker server
    modifyResponse . setHeader "Set-Token" . fromString . show . sToken $ session
    poll session

snapRequestURI :: Snap URI
snapRequestURI = do
    uri     <- getInput "info"
    maybe (error ("Unable to parse request URI: " ++ show uri)) return (uri >>= parseURI)

snapRequestCookies :: Snap [(String, String)]
snapRequestCookies = do
    cookies <- getsRequest rqCookies
    return $ flip map cookies $ \Cookie{..} -> (toString cookieName,toString cookieValue)


-- | Respond to poll requests.
poll :: Session -> Snap ()
poll Session{..} = do
    let setDisconnected = do
        now <- getCurrentTime
        modifyMVar_ sConnectedState (const (return (Disconnected now)))
    
    instructions <- liftIO $ do
        modifyMVar_ sConnectedState (const (return Connected))
        threadId <- myThreadId
        forkIO $ do
            delaySeconds $ 60 * 5 -- Force kill after 5 minutes.
            killThread threadId
        E.catch (readAvailableChan sInstructions) $ \e -> do
            -- no instructions available after some time
            when (e == Control.Exception.ThreadKilled) $ setDisconnected
            E.throw e
    
    writeJson instructions

-- | Handle signals sent from the client.
signal :: Session -> Snap ()
signal Session{..} = do
    input <- getParam "signal"
    let err = error $ "Unable to parse " ++ show input
    case JSON.decode . LBS.fromChunks . return =<< input of
        Just    signal -> liftIO $ writeChan sSignals signal
        Nothing        -> err

{-----------------------------------------------------------------------------
    Implementation of two-way communication
    - WebSockets
------------------------------------------------------------------------------}
-- | Route the communication between JavaScript and the server
routeWebsockets :: (Session -> IO a) -> ServerState -> Routes
routeWebsockets worker server =
    [("websocket", response)]
    where
    response = do
        session <- createSession worker server
        WS.runWebSocketsSnap (webSocket session)
        error "Threepenny.Internal.Core: runWebSocketsSnap should never return."


webSocket :: Session -> WS.PendingConnection -> IO ()
webSocket Session{..} request = void $ do
    connection <- WS.acceptRequest request
    -- websockets are always connected, don't let the custodian kill you.
    modifyMVar_ sConnectedState (const (return Connected))

    -- write data (in another thread)
    sendData <- forkIO . forever $ do
        x <- readChan sInstructions
        -- see note [Instruction strictness]
        WS.sendTextData connection . JSON.encode $ x

    -- read data
    let readData = do
            input <- WS.receiveData connection
            case input of
                "ping" -> WS.sendTextData connection . LBS.pack $ "pong"
                "quit" -> E.throw WS.ConnectionClosed
                input  -> case JSON.decode input of
                    Just signal -> writeChan sSignals signal
                    Nothing     -> E.throw $ Atto.ParseError [] $
                        "Threepenny.Internal.Core: Couldn't parse 'Signal' "
                        ++ show input
    
    forever readData `E.finally`
        (do
            killThread sendData             -- kill sending thread when done
            writeChan sSignals $ Quit ()    -- signal  Quit  event
        )

{- note [Instruction strictness]

The  Instruction  may contain components that evaluate to _|_.
An exception will be thrown when we try to send one of those to the browser.
However, the  WS.sendSink  function is called in a different thread
than where the faulty instruction was constructed.
We want to throw an exception in the latter thread.
Hence, we make sure that the  Instruction  is fully evaluated (deepseq)
before passing it to the thread that sends it to the web browser.

(Another, probably preferred, solution would be to make the  Instruction
data type fully strict by default.)

-}



{-----------------------------------------------------------------------------
    FFI implementation on top of the communication channel
------------------------------------------------------------------------------}
-- | Atomically execute the given computation in the context of a browser window
atomic :: Window -> IO a -> IO a
atomic window@(Session{..}) m = do
  takeMVar sMutex
  ret <- m
  putMVar sMutex ()
  return ret

-- | Send an instruction and read the signal response.
call :: Session -> Instruction -> (Signal -> IO (Maybe a)) -> IO a
call session@(Session{..}) instruction withSignal = do
  -- see note [Instruction strictness]
  Control.Exception.evaluate $ force instruction
  takeMVar sMutex
  writeChan sInstructions instruction
  newChan <- dupChan sSignals
  go sMutex newChan

  where
    go mutex newChan = do
      signal <- readChan newChan
      result <- withSignal signal
      case result of
        Just signal -> do putMVar mutex ()
                          return signal
        Nothing     -> go mutex newChan
            -- keep reading signals from the duplicated channel
            -- until the function above succeeds

-- Run the given instruction wihtout waiting for a response.
run :: Session -> Instruction -> IO ()
run (Session{..}) instruction =
    writeChan sInstructions $!! instruction  -- see note [Instruction strictness]

-- | Call the given function with the given continuation. Doesn't block.
callDeferredFunction
  :: Window                    -- ^ Browser window
  -> String                    -- ^ The function name.
  -> [String]                  -- ^ Parameters.
  -> ([Maybe String] -> IO ()) -- ^ The continuation to call if/when the function completes.
  -> IO ()
callDeferredFunction window fun params thunk = do
    closure <- newClosure window fun $ \(EventData xs) -> thunk xs
    run window $ CallDeferredFunction (closure,fun,params)

-- | Run the given JavaScript function and carry on. Doesn't block.
--
-- The client window uses JavaScript's @eval()@ function to run the code.
runFunction :: Window -> JSFunction () -> IO ()
runFunction session = run session . RunJSFunction . toCode

-- | Run the given JavaScript function and wait for results. Blocks.
--
-- The client window uses JavaScript's @eval()@ function to run the code.
callFunction :: Window -> JSFunction a -> IO a
callFunction window jsfunction = 
    call window (CallJSFunction . toCode $ jsfunction) $ \signal ->
        case signal of
            FunctionResult v -> case marshalResult jsfunction window v of
                Success a -> return $ Just a
                Error   _ -> return Nothing
            _ -> return Nothing


-- | Package a Haskell function such that it can be called from JavaScript.
-- 
-- At the moment, we implement this as an event handler that is
-- attached to the @head@ element.
newClosure
    :: Window               -- ^ Browser window context
    -> String               -- ^ Function name (for debugging).
    -> (EventData -> IO ()) -- ^ Function to call
    -> IO Closure
newClosure window@(Session{..}) fun thunk = do
    cid <- modifyMVar sClosures $ \(x:xs) -> return (xs,x)
    let eventId = fun ++ "-" ++ show cid
    attachClosure sHeadElement eventId thunk
    return $ Closure (unprotectedGetElementId sHeadElement, eventId)


{-----------------------------------------------------------------------------
    Snap utilities
------------------------------------------------------------------------------}
-- Write JSON to output.
writeJson :: (MonadSnap m, JSON.ToJSON a) => a -> m ()
writeJson json = do
    modifyResponse $ setContentType "application/json"
    writeLBS . JSON.encode $ json

-- Get a text input from snap.
getInput :: (MonadSnap f) => ByteString -> f (Maybe String)
getInput = fmap (fmap (unpack . Text.decodeUtf8)) . getParam

-- Read an input from snap.
readInput :: (MonadSnap f,Read a) => ByteString -> f (Maybe a)
readInput = fmap (>>= readMay) . getInput

{-----------------------------------------------------------------------------
    Resourcse
------------------------------------------------------------------------------}
type Routes = [(ByteString, Snap ())]

routeResources :: Maybe FilePath -> Maybe FilePath -> ServerState -> Routes
routeResources customHTML staticDir server =
    fixHandlers noCache $
        static ++
        [("/"                          , root)
        ,("/driver/threepenny-gui.js"  , writeText jsDriverCode )
        ,("/driver/threepenny-gui.css" , writeText cssDriverCode)
        ,("/file/:name"                ,
            withFilepath (sFiles server) (flip serveFileAs))
        ,("/dir/:name"                 ,
            withFilepath (sDirs  server) (\path _ -> serveDirectory path))
        ]
    where
    fixHandlers f routes = [(a,f b) | (a,b) <- routes]
    noCache h = modifyResponse (setHeader "Cache-Control" "no-cache") >> h
    
    static = maybe [] (\dir -> [("/static", serveDirectory dir)]) staticDir
    
    root = case customHTML of
        Just file -> case staticDir of
            Just dir -> serveFile (dir </> file)
            Nothing  -> logError "Graphics.UI.Threepenny: Cannot use tpCustomHTML file without tpStatic"
        Nothing   -> writeText defaultHtmlFile


-- Get a filename from a URI
withFilepath :: MVar Filepaths -> (FilePath -> MimeType -> Snap a) -> Snap a
withFilepath rDict cont = do
    mName    <- getParam "name"
    (_,dict) <- liftIO $ withMVar rDict return
    case (\key -> M.lookup key dict) =<< mName of
        Just (path,mimetype) -> cont path mimetype
        Nothing              -> error $ "File not loaded: " ++ show mName

-- FIXME: Serving large files fails with the exception
-- System.SendFile.Darwin: invalid argument (Socket is not connected)


newAssociation :: MVar Filepaths -> (FilePath, MimeType) -> IO String
newAssociation rDict (path,mimetype) = do
    (old, dict) <- takeMVar rDict
    let new = old + 1; key = show new ++ takeFileName path
    putMVar rDict $ (new, M.insert (fromString key) (path,mimetype) dict)
    return key

-- | Begin to serve a local file under an URI.
loadFile :: Session -> MimeType -> FilePath -> IO String
loadFile Session{..} mimetype path = do
    key <- newAssociation (sFiles sServerState) (path,mimetype)
    return $ "/file/" ++ key

-- | Begin to serve a local directory under an URI.
loadDirectory :: Session -> FilePath -> IO String
loadDirectory Session{..} path = do
    key <- newAssociation (sDirs sServerState) (path,"")
    return $ "/dir/" ++ key


{-----------------------------------------------------------------------------
    Elements
        Creation, Management, Finalization
------------------------------------------------------------------------------}
-- | Create a new element of the given tag name.
newElement :: Window        -- ^ Browser window in which context to create the element
           -> String        -- ^ The tag name.
           -> Events        -- ^ Events associated to that element.
           -> IO Element    -- ^ A tag reference. Non-blocking.
newElement elSession@(Session{..}) elTagName elEvents = do
    elHandlers <- newMVar M.empty
    el         <- Foreign.newItem sPrizeBooth ElementData{..}
    Foreign.addFinalizer el $ delete el
        -- FIXME: Do not try to delete elements from the session when
        -- the session is broken/disconnected already.
        -- A fix should be part of the  run  function, though.
    return el

-- | Get 'Window' associated to an 'Element'.
getWindow :: Element -> IO Window
getWindow e = withElement e $ \_ window -> return window

-- | Look up several elements in the browser window.
lookupElements :: Session -> [ElementId] -> IO [Element]
lookupElements window = mapM (flip lookupElement window)

-- | Append a child element to a parent element. Non-blocking.
appendElementTo
    :: Element     -- ^ Parent.
    -> Element     -- ^ Child.
    -> IO ()
appendElementTo eParent eChild =
    -- TODO: Right now, parent and child need to be from the same session/browser window
    --       Implement transfer of elements across browser windows
    withElement eParent $ \parent session ->
    withElement eChild  $ \child  _       -> do
        Foreign.addReachable eParent eChild
        runFunction session $ ffi "$(%1).append($(%2))" parent child

-- | Get the head of the page.
getHead :: Window -> IO Element
getHead session = return $ sHeadElement session

-- | Get the body of the page.
getBody :: Window -> IO Element
getBody session = return $ sBodyElement session

-- | Empty the given element.
emptyEl :: Element -> IO ()
emptyEl el = withElement el $ \elid window -> do
    Foreign.clearReachable el
    runFunction window $ ffi "$(%1).contents().detach()" elid

-- | Delete the given element.
delete :: Element -> IO ()
delete el = withElement el $ \elid window ->
    run window $ Delete elid
    -- Note: We want a primitive 'Delete' here, because 
    -- we do not want the implicit conversion from ElementId to element.


{-----------------------------------------------------------------------------
    Event handling
------------------------------------------------------------------------------}
{- $eventhandling

    To bind events to elements, use the 'bind' function.

    To handle DOM events, use the 'handleEvent' function, or the
    'handleEvents' function which will block forever.

    See the rest of this section for some helpful functions that do
    common binding, such as clicks, hovers, etc.
-}

-- | Handle events signalled from the client.
handleEvents :: Window -> IO ()
handleEvents window@(Session{..}) = do
    signal <- getSignal window
    case signal of
        Threepenny.Event elid eventId params -> do
            handleEvent1 window (elid,eventId,EventData params)
#ifdef REBUG
            -- debug garbage collection of elements:
            System.Mem.performGC
#endif
            handleEvents window
        Quit () -> do
            snd sEventQuit ()
            -- do not continue handling events
        _       -> do
            handleEvents window

-- | Handle a single event.
handleEvent1 :: Window -> (ElementId, EventId, EventData) -> IO ()
handleEvent1 window (elid,eventId,params) = do
    el <- lookupElement elid window
    withElementData el $ \_ eldata -> do
        handlers <- readMVar $ elHandlers eldata
        case M.lookup eventId handlers of
            Just handler -> handler params
            Nothing      -> return ()

-- Get the latest signal sent from the client.
getSignal :: Window -> IO Signal
getSignal (Session{..}) = readChan sSignals

-- | Associate a new closure with an element.
attachClosure :: Element -> EventId -> Handler EventData -> IO () 
attachClosure el eventId handler = withElementData el $ \_ eldata ->
    modifyMVar_ (elHandlers eldata) $ return .
        M.insertWith (\h1 h a -> h1 a >> h a) eventId handler

-- | Bind an event handler for a dom event to an 'Element'.
bind
    :: EventId              -- ^ The eventType, see any DOM documentation for a list of these.
    -> Element              -- ^ The element to bind to.
    -> Handler EventData    -- ^ The event handler to bind.
    -> IO ()
bind eventId e handler = withElementData e $ \elid el -> do
    handlers <- readMVar $ elHandlers el
    -- register with client if it has never been registered on the server
    when (not $ eventId `M.member` handlers) $
        run (elSession el) $ Bind eventId elid
    -- register with server
    attachClosure e eventId handler

-- | Register event handler that occurs when the client has disconnected.
disconnect :: Window -> Event ()
disconnect = fst . sEventQuit

-- | Initialize the 'head' and 'body' elements when the session starts.
initializeElements :: Session -> IO Session
initializeElements session@(Session{..}) = do
        sHeadElement <- createElement "head"
        sBodyElement <- createElement "body"
        return $ Session{..}
    where
    newEvents     e   = newEventsNamed $ \(name,_,handler) -> bind name e handler
    createElement tag = mdo
        x <- newElement session tag =<< newEvents x
        return x

{-----------------------------------------------------------------------------
    Querying the DOM
------------------------------------------------------------------------------}
-- $querying
-- 
-- The DOM can be searched for elements of a given name, and nodes can
-- be inspected for their values.

-- | Get all elements of the given tag name. Blocks.
getElementsByTagName :: Window -> String -> IO [Element]
getElementsByTagName window tag = do
    elids <- callFunction window $ ffi "document.getElementsByTagName(%1)" tag
    lookupElements window elids

-- | Get a list of elements by particular IDs.  Blocks.
getElementsById :: Window -> [String] -> IO [Element]
getElementsById window ids = do
    elids <- forM ids $ \x ->
        callFunction window $ ffi "[document.getElementById(%1)]" x
    lookupElements window $ concat elids

-- | Get a list of elements by particular class.  Blocks.
getElementsByClassName :: Window -> String -> IO [Element]
getElementsByClassName window cls = do
    elids <- callFunction window $ ffi "document.getElementsByClassName(%1)" cls
    lookupElements window elids

-- | Get values from inputs. Blocks. This is faster than many @getValue@ invocations.
getValuesList
    :: [Element]   -- ^ A list of elements to get the values of.
    -> IO [String] -- ^ The list of plain text values.
getValuesList []        = return []
getValuesList es@(e0:_) = withElement e0 $ \_ window -> do
    let elids  = map unprotectedGetElementId es
    call window (GetValues elids) $ \signal ->
        case signal of
            Values strs -> return $ Just strs
            _           -> return Nothing

-- | Get the request location.
getRequestLocation :: Window -> IO URI
getRequestLocation = return . fst . sStartInfo

-- | Get the request cookies.
getRequestCookies :: Window -> IO [(String,String)]
getRequestCookies = return . snd . sStartInfo

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
-- | Send a debug message to the client. The behaviour of the client
--   is unspecified.
debug
    :: Window -- ^ Client window
    -> String -- ^ Some plain text to send to the client.
    -> IO ()
debug window = run window . Debug
