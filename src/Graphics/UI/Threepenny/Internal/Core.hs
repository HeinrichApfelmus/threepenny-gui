{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings, PackageImports #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Graphics.UI.Threepenny.Internal.Core
  (
  -- * Synopsis
  -- | The main internal functionality.
      
  -- * Server running
   serve
  ,loadFile
  ,loadDirectory
  
  -- * Event handling
  -- $eventhandling
  ,bind
  ,disconnect
  ,module Reactive.Threepenny
  
  -- * Setting attributes
  -- $settingattributes
  ,setStyle
  ,setAttr
  ,setProp
  ,setText
  ,setHtml
  ,setTitle
  ,emptyEl
  ,delete
  
  -- * Manipulating tree structure
  -- $treestructure
  ,newElement
  ,appendElementTo
  
  -- * Querying
  -- $querying
  ,getHead
  ,getBody
  ,getElementsByTagName
  ,getElementsById
  ,getElementsByClassName
  ,getWindow
  ,getProp
  ,getValue
  ,getValuesList
  ,readValue
  ,readValuesList
  ,getRequestCookies
  ,getRequestLocation
  
  -- * Utilities
  ,debug
  ,clear
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



import           Graphics.UI.Threepenny.Internal.Types     as Threepenny
import           Graphics.UI.Threepenny.Internal.Resources

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Chan.Extra
import           Control.Concurrent.Delay
import qualified Control.Exception
import           Reactive.Threepenny
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
import           Data.Text.Encoding
import           Data.Time
import           Network.URI
import qualified Network.WebSockets            as WS
import qualified Network.WebSockets.Snap       as WS
import qualified Data.Attoparsec.Enumerator    as Atto
import           Prelude                       hiding (init)
import           Safe
import           Snap.Core
import           Snap.Http.Server              hiding (Config)
import           Snap.Util.FileServe
import           System.FilePath
import qualified Text.JSON as JSON
import           Text.JSON.Generic


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
    server <- newServerState
    _      <- forkIO $ custodian 30 (sSessions server)
    let config = setPort tpPort defaultConfig
    httpServe config . route $
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
    sEventHandlers    <- newMVar M.empty
    sElementEvents    <- newMVar M.empty
    sEventQuit        <- newEvent
    sElementIds       <- newMVar [0..]
    now               <- getCurrentTime
    sConnectedState   <- newMVar (Disconnected now)
    sThreadId         <- myThreadId
    sClosures         <- newMVar [0..]
    let session = Session {..}
    initializeElementEvents session    
    return session

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
    input <- getInput "signal"
    case input of
      Just signalJson -> do
        let signal = JSON.decode signalJson
        case signal of
          Ok signal -> liftIO $ writeChan sSignals signal
          Error err -> error err
      Nothing -> error $ "Unable to parse " ++ show input

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


webSocket :: Session -> WS.Request -> WS.WebSockets WS.Hybi00 ()
webSocket Session{..} req = void $ do
    WS.acceptRequest req
    -- websockets are always connected, don't let the custodian kill you.
    liftIO $ modifyMVar_ sConnectedState (const (return Connected))

    -- write data (in another thread)
    send     <- WS.getSink
    sendData <- liftIO . forkIO . forever $ do
        x <- readChan sInstructions
        WS.sendSink send . WS.textData . Text.pack . JSON.encode $ x

    -- read data
    let readData = do
            input <- WS.receiveData
            case input of
                "ping" -> liftIO . WS.sendSink send . WS.textData . Text.pack $ "pong"
                "quit" -> WS.throwWsError WS.ConnectionClosed
                input  -> case JSON.decode . Text.unpack $ input of
                    Ok signal -> liftIO $ writeChan sSignals signal
                    Error err -> WS.throwWsError . WS.ParseError $ Atto.ParseError [] err
    
    forever readData `WS.catchWsError`
        \_ -> liftIO $ do
            killThread sendData             -- kill sending thread when done
            writeChan sSignals $ Quit ()    -- signal  Quit  event

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
  takeMVar sMutex
  run session $ instruction
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
run (Session{..}) i = writeChan sInstructions i

-- | Call the given function with the given continuation. Doesn't block.
callDeferredFunction
  :: Window                    -- ^ Browser window
  -> String                    -- ^ The function name.
  -> [String]                  -- ^ Parameters.
  -> ([Maybe String] -> IO ()) -- ^ The continuation to call if/when the function completes.
  -> IO ()
callDeferredFunction session@(Session{..}) func params closure = do
  cid      <- modifyMVar sClosures (\(x:xs) -> return (xs,x))
  closure' <- newClosure session func (show cid) closure
  run session $ CallDeferredFunction (closure',func,params)

-- | Run the given JavaScript function and carry on. Doesn't block.
--
-- The client window uses JavaScript's @eval()@ function to run the code.
runFunction :: Window -> JSFunction () -> IO ()
runFunction session = run session . RunJSFunction . unJSCode . code

-- | Run the given JavaScript function and wait for results. Blocks.
--
-- The client window uses JavaScript's @eval()@ function to run the code.
callFunction :: Window -> JSFunction a -> IO a
callFunction window (JSFunction code marshal) = 
    call window (CallJSFunction . unJSCode $ code) $ \signal ->
        case signal of
            FunctionResult v -> case marshal window v of
                Ok    a -> return $ Just a
                Error _ -> return Nothing
            _ -> return Nothing


{-----------------------------------------------------------------------------
    Snap utilities
------------------------------------------------------------------------------}
-- Write JSON to output.
writeJson :: (MonadSnap m, JSON a) => a -> m ()
writeJson json = do
    modifyResponse $ setContentType "application/json"
    (writeText . pack . (\x -> showJSValue x "") . showJSON) json

-- Get a text input from snap.
getInput :: (MonadSnap f) => ByteString -> f (Maybe String)
getInput = fmap (fmap (unpack . decodeUtf8)) . getParam

-- Read an input from snap.
readInput :: (MonadSnap f,Read a) => ByteString -> f (Maybe a)
readInput = fmap (>>= readMay) . getInput

{-----------------------------------------------------------------------------
    Resourcse
------------------------------------------------------------------------------}
type Routes = [(ByteString, Snap ())]

routeResources :: Maybe FilePath -> FilePath -> ServerState -> Routes
routeResources customHTML staticDir server =
    fixHandlers noCache $
        [("/static"                    , serveDirectory staticDir)
        ,("/"                          , root)
        ,("/driver/threepenny-gui.js"  , writeText jsDriverCode )
        ,("/driver/threepenny-gui.css" , writeText cssDriverCode)
        ,("/file/:name"                ,
            withFilepath (sFiles server) (flip serveFileAs))
        ,("/dir/:name"                 ,
            withFilepath (sDirs  server) (\path _ -> serveDirectory path))
        ]
    where
    fixHandlers f routes = [(a,f b) | (a,b) <- routes]
    noCache h = do
        modifyResponse $ setHeader "Cache-Control" "no-cache"
        h
    
    root = case customHTML of
        Just file -> serveFile (staticDir </> file)
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
        Threepenny.Event (elid,eventType,params) -> do
            handleEvent1 window ((elid,eventType),EventData params)
            handleEvents window
        Quit () -> do
            snd sEventQuit ()
            -- do not continue handling events
        _       -> do
            handleEvents window

-- | Add a new event handler for a given key
addEventHandler :: Window -> (EventKey, Handler EventData) -> IO () 
addEventHandler Session{..} (key,handler) =
    modifyMVar_ sEventHandlers $ return .
        M.insertWith (\h1 h a -> h1 a >> h a) key handler        


-- | Handle a single event
handleEvent1 :: Window -> (EventKey,EventData) -> IO ()
handleEvent1 Session{..} (key,params) = do
    handlers <- readMVar sEventHandlers
    case M.lookup key handlers of
        Just handler -> handler params
        Nothing      -> return ()

-- Get the latest signal sent from the client.
getSignal :: Window -> IO Signal
getSignal (Session{..}) = readChan sSignals

-- | Bind an event handler for a dom event to an 'Element'.
bind
    :: String               -- ^ The eventType, see any DOM documentation for a list of these.
    -> Element              -- ^ The element to bind to.
    -> Handler EventData    -- ^ The event handler to bind.
    -> IO ()
bind eventType (Element el@(ElementId elid) session) handler = do
    let key = (elid, eventType)
    -- register with client if it has never been registered on the server
    handlers <- readMVar $ sEventHandlers session
    when (not $ key `M.member` handlers) $
        run session $ Bind eventType el (Closure key)
    -- register with server
    addEventHandler session (key, handler)

-- | Register event handler that occurs when the client has disconnected.
disconnect :: Window -> Event ()
disconnect = fst . sEventQuit


initializeElementEvents :: Window -> IO ()
initializeElementEvents session@(Session{..}) = do
        initEvents =<< getHead session
        initEvents =<< getBody session
    where
    initEvents el@(Element elid _) = do
        x <- newEventsNamed $ \(name,_,handler) -> bind name el handler
        modifyMVar_ sElementEvents $ return . M.insert elid x

-- Make a uniquely numbered event handler.
newClosure :: Window -> String -> String -> ([Maybe String] -> IO ()) -> IO Closure
newClosure window eventType elid thunk = do
    let key = (elid, eventType)
    addEventHandler window (key, \(EventData xs) -> thunk xs)
    return (Closure key)

{-----------------------------------------------------------------------------
    Setting attributes
------------------------------------------------------------------------------}
{- $settingattributes
 
    Text, HTML and attributes of DOM nodes can be set using the
    functions in this section. 
-}

-- | Set the style of the given element.
setStyle :: [(String, String)] -- ^ Pairs of CSS (property,value).
         -> Element            -- ^ The element to update.
         -> IO ()
setStyle props e@(Element el session) = run session $ SetStyle el props

-- | Set the attribute of the given element.
setAttr :: String  -- ^ The attribute name.
        -> String  -- ^ The attribute value.
        -> Element -- ^ The element to update.
        -> IO ()
setAttr key value e@(Element el session) = run session $ SetAttr el key value

-- | Set the property of the given element.
setProp :: String  -- ^ The property name.
        -> JSValue -- ^ The property value.
        -> Element -- ^ The element to update.
        -> IO ()
setProp key value e@(Element el session) =
    runFunction session $ ffi "$(%1).prop(%2,%3);" el key value

-- | Set the text of the given element.
setText :: String  -- ^ The plain text.
        -> Element -- ^ The element to update.
        -> IO ()
setText props e@(Element el session) = run session $ SetText el props

-- | Set the HTML of the given element.
setHtml :: String  -- ^ The HTML.
        -> Element -- ^ The element to update.
        -> IO ()
setHtml props e@(Element el session) = run session $ SetHtml el props

-- | Set the title of the document.
setTitle
    :: String  -- ^ The title.
    -> Window  -- ^ The document window
    -> IO ()
setTitle title session = run session $ SetTitle title

-- | Empty the given element.
emptyEl :: Element -> IO ()
emptyEl e@(Element el session) = run session $ EmptyEl el

-- | Delete the given element.
delete :: Element -> IO ()
delete e@(Element el session)  = run session $ Delete el


{-----------------------------------------------------------------------------
    Manipulating tree structure
------------------------------------------------------------------------------}
-- $treestructure
--
--  Functions for creating, deleting, moving, appending, prepending, DOM nodes.

-- | Create a new element of the given tag name.
newElement :: Window      -- ^ Browser window in which context to create the element
           -> String      -- ^ The tag name.
           -> IO Element  -- ^ A tag reference. Non-blocking.
newElement session@(Session{..}) tagName = do
    elid <- modifyMVar sElementIds $ \elids ->
        return (tail elids,"*" ++ show (head elids) ++ ":" ++ tagName)
    return (Element (ElementId elid) session)

-- | Append a child element to a parent element. Non-blocking.
appendElementTo
    :: Element     -- ^ Parent.
    -> Element     -- ^ Child.
    -> IO () 
appendElementTo (Element parent session) e@(Element child _) =
    -- TODO: Right now, parent and child need to be from the same session/browser window
    --       Implement transfer of elements across browser windows
    run session $ Append parent child

{-----------------------------------------------------------------------------
    Querying the DOM
------------------------------------------------------------------------------}
-- $querying
-- 
-- The DOM can be searched for elements of a given name, and nodes can
-- be inspected for their values.

-- | Get all elements of the given tag name.  Blocks.
getElementsByTagName
    :: Window        -- ^ Browser window
    -> String        -- ^ The tag name.
    -> IO [Element]  -- ^ All elements with that tag name.
getElementsByTagName window tagName =
  call window (GetElementsByTagName tagName) $ \signal ->
    case signal of
      Elements els -> return $ Just $ [Element el window | el <- els]
      _            -> return Nothing

-- | Get a list of elements by particular IDs.  Blocks.
getElementsById
    :: Window        -- ^ Browser window
    -> [String]      -- ^ The ID string.
    -> IO [Element]  -- ^ Elements with given ID.
getElementsById window ids =
  call window (GetElementsById ids) $ \signal ->
    case signal of
      Elements els -> return $ Just [Element el window | el <- els]
      _            -> return Nothing

-- | Get a list of elements by particular class.  Blocks.
getElementsByClassName
    :: Window        -- ^ Browser window
    -> String        -- ^ The class string.
    -> IO [Element]  -- ^ Elements with given class.
getElementsByClassName window cls =
  call window (GetElementsByClassName cls) $ \signal ->
    case signal of
      Elements els -> return $ Just [Element el window | el <- els]
      _            -> return Nothing

-- | Get the value of an input. Blocks.
getValue
    :: Element   -- ^ The element to get the value of.
    -> IO String -- ^ The plain text value.
getValue e@(Element el window) =
  call window (GetValue el) $ \signal ->
    case signal of
      Value str -> return (Just str)
      _         -> return Nothing

-- | Get the property of an element. Blocks.
getProp
    :: String     -- ^ The property name.
    -> Element    -- ^ The element to get the value of.
    -> IO JSValue -- ^ The plain text value.
getProp prop e@(Element el window) =
    callFunction window (ffi "$(%1).prop(%2)" el prop)

-- | Get 'Window' associated to an 'Element'.
getWindow :: Element -> Window
getWindow (Element _ window) = window

-- | Get values from inputs. Blocks. This is faster than many 'getValue' invocations.
getValuesList
    :: [Element]   -- ^ A list of elements to get the values of.
    -> IO [String] -- ^ The list of plain text values.
getValuesList [] = return []
getValuesList es@(Element _ window : _) =
    let elids = [elid | Element elid _ <- es] in
    call window (GetValues elids) $ \signal ->
        case signal of
            Values strs -> return $ Just strs
            _           -> return Nothing

-- | Read a value from an input. Blocks.
readValue
    :: Read a
    => Element      -- ^ The element to read a value from.
    -> IO (Maybe a) -- ^ Maybe the read value.
readValue = liftM readMay . getValue

-- | Read values from inputs. Blocks. This is faster than many 'readValue' invocations.
readValuesList
    :: Read a
    => [Element]      -- ^ The element to read a value from.
    -> IO (Maybe [a]) -- ^ Maybe the read values. All or none.
readValuesList = liftM (sequence . map readMay) . getValuesList

-- | Get the head of the page.
getHead :: Window -> IO Element
getHead session = return $ Element (ElementId "head") session

-- | Get the body of the page.
getBody :: Window -> IO Element
getBody session = return $ Element (ElementId "body") session

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

-- | Clear the client's DOM.
clear :: Window -> IO ()
clear window = runFunction window $ ffi "$('body').contents().detach()"
