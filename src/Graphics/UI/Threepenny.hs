{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The main Ji module.


--------------------------------------------------------------------------------
-- Exports

module Graphics.UI.Threepenny
  (
  -- * Guide
  -- $guide
  
  -- * Server running
  -- $serverrunning
   serve
  ,runJi
  
  -- * Event handling
  -- $eventhandling
  ,bind
  ,handleEvent
  ,handleEvents
  ,onClick
  ,onHover
  ,onBlur
  
  -- * Setting attributes
  -- $settingattributes
  ,setStyle
  ,setAttr
  ,setText
  ,setHtml
  ,setTitle
  ,emptyEl
  ,delete
  
  -- * Manipulating tree structure
  -- $treestructure
  ,newElement
  ,appendTo
  
  -- * Querying
  -- $querying
  ,getHead
  ,getBody
  ,getElementsByTagName
  ,getElementByTagName
  ,getValue
  ,getValuesList
  ,readValue
  ,readValuesList
  ,getRequestCookies
  ,getRequestLocation
  
  -- * Utilities
  -- $utilities
  ,debug
  ,clear
  ,callFunction
  ,runFunction
  ,callDeferredFunction
  ,atomic
  ,forkJi
  
  -- * Types
  ,module Graphics.UI.Threepenny.Types)
  where


--------------------------------------------------------------------------------
-- Imports

import           Graphics.UI.Threepenny.Types
import           Graphics.UI.Threepenny.Internal.Types
import           Graphics.UI.Threepenny.Internal.Include

import           Control.Concurrent
import           Control.Concurrent.Chan.Extra
import           Control.Concurrent.Delay
import qualified Control.Exception             as E
import           Control.Monad.IO
import           Control.Monad.Reader
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
import           Prelude                       hiding (init)
import           Safe
import           Snap.Core
import           Snap.Http.Server              hiding (Config)
import           Snap.Util.FileServe
import           System.FilePath
import           Text.JSON.Generic


--------------------------------------------------------------------------------
-- Guide
--
-- $guide
--
-- Ji consists of a bit of JavaScript that you can find in the
-- wwwroot/js directory in this package, and a server. See the next
-- section for how to run the server.
--
-- Applications written in Ji are multithreaded. Each client (user)
-- has a separate thread which runs with no awareness of the asynchronous
-- protocol below. Each session should only be accessed from one
-- thread. There is not yet any clever architecture for accessing the
-- (single threaded) web browser from multi-threaded Haskell. That's
-- my recommendation. You can choose to ignore it, but don't blame me
-- when you run an element search and you get a click event as a
-- result.
--
-- The DOM is accessed much in the same way it is accessed from
-- JavaScript; elements can be created, searched, updated, moved and
-- inspected. Events can be bound to DOM elements and handled.


--------------------------------------------------------------------------------
-- Server running
-- 
-- $serverrunning
-- 
-- The server runs using Snap (for now), run it using 'serve' and you
-- must provide it a function to run the session monad. The session
-- monad is any monad with access to the current session. You can just
-- use 'runJi' if you don't need to subclass 'MonadJi'.

-- | Run a Ji server with Snap on the specified port and the given
--   worker action.
serve :: MonadJi m => Config m a -> IO () -- ^ A Ji server.
serve Config{..} = do
  sessions <- newMVar M.empty
  _ <- forkIO $ custodian 30 sessions
  httpServe server (router jiInitHTML jiStatic (\session -> jiRun session jiWorker) sessions)
 where server = setPort jiPort defaultConfig

-- | Kill sessions after at least n seconds of disconnectedness.
custodian :: Integer -> MVar (Map Integer (Session m)) -> IO ()
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
          if (dcSeconds > fromIntegral seconds)
             then do killThread sThreadId
                     return (Just key)
             else return Nothing
            
    return (M.filterWithKey (\k _ -> not (k `elem` killed)) sessions)

-- | Convenient way to a run a worker with a session.
runJi :: Session Ji  -- ^ The browser session.
      -> Ji a       -- ^ The worker.
      -> IO a       -- ^ A worker runner.
runJi session m = runReaderT (getJi m) session

-- Route requests.
router
    :: Maybe FilePath -> FilePath -> (Session m -> IO a)
    -> MVar (Map Integer (Session m)) -> Snap ()
router initFile wwwroot worker sessions =
        route [("/static"               , serveDirectory wwwroot)
              ,("/"                     , root)
              ,("/js/threepenny-gui.js" , writeText jsDriverCode)
              ,("/init"                 , init worker sessions)
              ,("/poll"                 , poll sessions)
              ,("/signal"               , signal sessions)
              ]
    where
    root = case initFile of
        Just file -> serveFile (wwwroot </> file)
        Nothing   -> writeText defaultHtmlFile

jsDriverCode :: Text
jsDriverCode = Text.unlines $ map Text.pack
    [ [include|Graphics/UI/jquery.js|]
    , [include|Graphics/UI/jquery.cookie.js|]
    , [include|Graphics/UI/driver.js|]
    ]

defaultHtmlFile :: Text
defaultHtmlFile = Text.pack [include|Graphics/UI/index.html|]


-- Initialize the session.
init :: (Session m -> IO void) -> MVar (Map Integer (Session m)) -> Snap ()
init sessionThread sessions = do
  uri <- getRequestURI
  params <- getRequestCookies
  key <- io $ modifyMVar sessions $ \sessions -> do
    let newKey = maybe 0 (+1) (lastMay (M.keys sessions))
    session <- newSession (uri,params) newKey
    _ <- forkIO $ do _ <- sessionThread session; return ()
    return (M.insert newKey session sessions,newKey)
  modifyResponse $ setHeader "Set-Token" (fromString (show key))
  withGivenSession key sessions pollWithSession
    
  where getRequestURI = do
          uri <- getInput "info"
          maybe (error ("Unable to parse request URI: " ++ show uri)) return (uri >>= parseURI)
        getRequestCookies = do
          cookies <- getsRequest rqCookies
          return $ flip map cookies $ \Cookie{..} -> (toString cookieName,toString cookieValue)

-- Make a new session.
newSession :: (URI,[(String, String)]) -> Integer -> IO (Session m)
newSession info token = do
  signals <- newChan
  instructions <- newChan
  handlers <- newMVar M.empty
  ids <- newMVar [0..]
  mutex <- newMVar ()
  now <- getCurrentTime
  conState <- newMVar (Disconnected now)
  threadId <- myThreadId
  closures <- newMVar [0..]
  return $ Session
    { sSignals = signals
    , sInstructions = instructions
    , sEventHandlers = handlers
    , sElementIds = ids
    , sToken = token
    , sMutex = mutex
    , sConnectedState = conState
    , sThreadId = threadId
    , sClosures = closures
    , sStartInfo = info
    }

-- Respond to poll requests.
poll :: MVar (Map Integer (Session m)) -> Snap ()
poll sessions = withSession sessions pollWithSession
  
-- Poll for the given session.
pollWithSession :: Session m -> Snap ()
pollWithSession Session{..} = do
  let setDisconnected = do
        now <- getCurrentTime
        modifyMVar_ sConnectedState (const (return (Disconnected now)))
  io $ modifyMVar_ sConnectedState (const (return Connected))
  threadId <- io $ myThreadId
  _ <- io $ forkIO $ do
    delaySeconds $ 60 * 5 -- Force kill after 5 minutes.
    killThread threadId
  instructions <- io $ E.catch (readAvailableChan sInstructions) $ \e -> do
    when (e == E.ThreadKilled) $ do
      setDisconnected
    E.throw e
  writeJson instructions

-- Write JSON to output.
writeJson :: (MonadSnap m, Data a) => a -> m ()
writeJson json = do
    modifyResponse $ setContentType "application/json"
    (writeString . encodeJSON) json

-- Write a string to output.
writeString :: (MonadSnap m) => String -> m ()
writeString = writeText . pack

-- Handle signals sent from the client.
signal :: MVar (Map Integer (Session m)) -> Snap ()
signal sessions = do
  withSession sessions $ \Session{..} -> do
    input <- getInput "signal"
    case input of
      Just signalJson -> do
        let signal = decode signalJson
        case signal of
          Ok signal -> io $ writeChan sSignals signal
          Error err -> error err
      Nothing -> error $ "Unable to parse " ++ show input

-- Get a text input from snap.
getInput :: (MonadSnap f) => ByteString -> f (Maybe String)
getInput = fmap (fmap (unpack . decodeUtf8)) . getParam

-- Run a snap action with the given session.
withSession :: MVar (Map Integer session) -> (session -> Snap a) -> Snap a
withSession sessions cont = do
  token <- readInput "token"
  case token of
    Nothing -> error $ "Invalid session token format."
    Just token -> withGivenSession token sessions cont
    
-- Do something with the session given by its token id.
withGivenSession :: Integer -> MVar (Map Integer session) -> (session -> Snap a) -> Snap a
withGivenSession token sessions cont = do
  sessions <- io $ withMVar sessions return
  case M.lookup token sessions of
    Nothing -> error $ "Nonexistant token: " ++ show token
    Just session -> cont session

-- Read an input from snap.
readInput :: (MonadSnap f,Read a) => ByteString -> f (Maybe a)
readInput = fmap (>>= readMay) . getInput


--------------------------------------------------------------------------------
-- Event handling
-- 
-- $eventhandling
-- 
-- To bind events to elements, use the 'bind' function.
--
-- To handle DOM events, use the 'handleEvent' function, or the
-- 'handleEvents' function which will block forever.
--
-- See the rest of this section for some helpful functions that do
-- common binding, such as clicks, hovers, etc.

-- | Handle events signalled from the client.
handleEvents :: MonadJi m => m ()
handleEvents = forever handleEvent

-- | Handle one event.
handleEvent :: MonadJi m => m ()
handleEvent = do
  signal <- get
  case signal of
    Event (elid,eventType,params) -> do
      Session{..} <- askSession
      handlers <- io $ withMVar sEventHandlers return
      case M.lookup (elid,eventType) handlers of
        Nothing -> return ()
        Just handler -> handler params
    _ -> return ()

-- Get the latest signal sent from the client.
get :: MonadJi m => m Signal
get = do
  Session{..} <- askSession
  io $ readChan sSignals

-- | Bind an event handler to the given event of the given element.
bind :: MonadJi m
     => String              -- ^ The eventType, see any DOM documentation for a list of these.
     -> Element             -- ^ The element to bind to.
     -> (EventData -> m ()) -- ^ The event handler.
     -> m ()
bind eventType (Element el) handler = do
  closure <- newClosure eventType el (\xs -> handler (EventData xs))
  run $ Bind eventType el closure

-- Make a uniquely numbered event handler.
newClosure :: MonadJi m => String -> String -> ([Maybe String] -> m ()) -> m Closure
newClosure eventType elid thunk = do
  Session{..} <- askSession
  io $ modifyMVar_ sEventHandlers $ \handlers -> do
    return (M.insert key thunk handlers)
  return (Closure key)
    
  where key = (elid,eventType)

-- | Bind an event handler to the click event of the given element.
onClick :: MonadJi m
        => Element             -- ^ The element to bind to.
        -> (EventData -> m ()) -- ^ The event handler.
        -> m ()
onClick = bind "click"

-- | Bind an event handler to the hover event of the given element.
onHover :: MonadJi m
        => Element             -- ^ The element to bind to.
        -> (EventData -> m ()) -- ^ The event handler.
        -> m ()
onHover = bind "mouseenter"

-- | Bind an event handler to the blur event of the given element.
onBlur :: MonadJi m
       => Element             -- ^ The element to bind to.
       -> (EventData -> m ()) -- ^ The event handler.
       -> m ()
onBlur = bind "mouseleave"


--------------------------------------------------------------------------------
-- Setting attributes
--
-- $settingattributes
-- 
-- Text, HTML and attributes of DOM nodes can be set using the
-- functions in this section. 

-- | Set the style of the given element.
setStyle :: (MonadJi m)
         => [(String, String)] -- ^ Pairs of CSS (property,value).
         -> Element            -- ^ The element to update.
         -> m Element
setStyle props el = do
  run $ SetStyle el props
  return el

-- | Set the attribute of the given element.
setAttr :: (MonadJi m)
        => String  -- ^ The attribute name.
        -> String  -- ^ The attribute value.p
        -> Element -- ^ The element to update.
        -> m Element
setAttr key value el = do
  run $ SetAttr el key value
  return el

-- | Set the text of the given element.
setText :: (MonadJi m)
        => String  -- ^ The plain text.
        -> Element -- ^ The element to update.
        -> m Element
setText props el = do
  run $ SetText el props
  return el

-- | Set the HTML of the given element.
setHtml :: (MonadJi m)
        => String  -- ^ The HTML.
        -> Element -- ^ The element to update.
        -> m Element
setHtml props el = do
  run $ SetHtml el props
  return el

-- | Set the title of the document.
setTitle :: (MonadJi m)
        => String  -- ^ The title.
        -> m ()
setTitle title = run $ SetTitle title

-- | Empty the given element.
emptyEl :: MonadJi m => Element -> m Element
emptyEl el = do
  run $ EmptyEl el
  return el

-- | Delete the given element.
delete :: MonadJi m => Element -> m ()
delete el = do
  run $ Delete el


--------------------------------------------------------------------------------
-- Manipulating tree structure
--
-- $treestructure
-- 
-- Functions for creating, deleting, moving, appending, prepending, DOM nodes.

-- | Create a new element of the given tag name.
newElement :: MonadJi m
           => String     -- ^ The tag name.
           -> m Element  -- ^ A tag reference. Non-blocking.
newElement tagName = do
  Session{..} <- askSession
  elid <- io $ modifyMVar sElementIds $ \elids ->
    return (tail elids,"*" ++ show (head elids) ++ ":" ++ tagName)
  return (Element elid)

-- | Append one element to another. Non-blocking.
appendTo :: (MonadJi m)
       => Element -- ^ The parent.
       -> Element -- ^ The resulting child.
       -> m Element
appendTo parent child = do
  run $ Append parent child
  return child


--------------------------------------------------------------------------------
-- Querying the DOM
--
-- $querying
-- 
-- The DOM can be searched for elements of a given name, and nodes can
-- be inspected for their values.

-- | Get an element by its tag name.  Blocks.
getElementByTagName
  :: MonadJi m
  => String            -- ^ The tag name.
  -> m (Maybe Element) -- ^ An element (if any) with that tag name.
getElementByTagName = liftM listToMaybe . getElementsByTagName

-- | Get all elements of the given tag name.  Blocks.
getElementsByTagName
  :: MonadJi m
  => String       -- ^ The tag name.
  -> m [Element]  -- ^ All elements with that tag name.
getElementsByTagName tagName =
  call (GetElementsByTagName tagName) $ \signal ->
    case signal of
      Elements els -> return (Just els)
      _            -> return Nothing

-- | Get the value of an input. Blocks.
getValue :: MonadJi m
         => Element  -- ^ The element to get the value of.
         -> m String -- ^ The plain text value.
getValue el =
  call (GetValue el) $ \signal ->
    case signal of
      Value str -> return (Just str)
      _         -> return Nothing

-- | Get values from inputs. Blocks. This is faster than many 'getValue' invocations.
getValuesList :: MonadJi m
              => [Element]  -- ^ A list of elements to get the values of.
              -> m [String] -- ^ The list of plain text values.
getValuesList el =
  call (GetValues el) $ \signal ->
    case signal of
      Values strs -> return (Just strs)
      _           -> return Nothing

-- | Read a value from an input. Blocks.
readValue :: (MonadJi m,Read a)
          => Element     -- ^ The element to read a value from.
          -> m (Maybe a) -- ^ Maybe the read value.
readValue = liftM readMay . getValue

-- | Read values from inputs. Blocks. This is faster than many 'readValue' invocations.
readValuesList :: (MonadJi m,Read a)
               => [Element]     -- ^ The element to read a value from.
               -> m (Maybe [a]) -- ^ Maybe the read values. All or none.
readValuesList = liftM (sequence . map readMay) . getValuesList

-- | Atomically execute the given Ji computation.
atomic :: MonadJi m => m a -> m a
atomic m = do
  Session {..} <- askSession
  io $ takeMVar sMutex
  ret <- m
  io $ putMVar sMutex ()
  return ret

-- | Fork the current Ji thread.
forkJi :: Ji a -> Ji ThreadId
forkJi x = do
  session <- askSession
  io . forkIO . runJi session $ x >> return ()

-- Send an instruction and read the signal response.
call :: MonadJi m => Instruction -> (Signal -> m (Maybe a)) -> m a
call instruction withSignal = do
  Session{..} <- askSession
  io $ takeMVar sMutex
  run $ instruction
  newChan <- io $ dupChan sSignals
  go sMutex newChan

  where
    go mutex newChan = do
      signal <- io $ readChan newChan
      result <- withSignal signal
      case result of
        Just signal -> do io $ putMVar mutex ()
                          return signal
        Nothing     -> go mutex newChan

-- Run the given instruction.
run :: MonadJi m => Instruction -> m ()
run i = do
  Session{..} <- askSession
  io $ writeChan sInstructions i

-- | Get the head of the page.
getHead :: MonadJi m => m Element
getHead = return (Element "head")

-- | Get the body of the page.
getBody :: MonadJi m => m Element
getBody = return (Element "body")

-- | Get the request location.
getRequestLocation :: MonadJi m => m URI
getRequestLocation = liftM (fst . sStartInfo) askSession

-- | Get the request cookies.
getRequestCookies :: MonadJi m => m [(String,String)]
getRequestCookies = liftM (snd . sStartInfo) askSession


--------------------------------------------------------------------------------
-- Utilities
--
-- $utilities
-- 
-- Some possibly helpful miscellaneous utilities.

-- | Send a debug message to the client. The behaviour of the client
--   is unspecified.
debug :: MonadJi m
      => String -- ^ Some plain text to send to the client.
      -> m ()
debug = run . Debug

-- | Clear the client's DOM.
clear :: MonadJi m => m ()
clear = run $ Clear ()

-- | Call the given function. Blocks.
callFunction
  :: MonadJi m
  => String            -- ^ The function name.
  -> [String]          -- ^ Parameters.
  -> m [Maybe String]  -- ^ Return tuple.
callFunction func params =
  call (CallFunction (func,params)) $ \signal ->
    case signal of
      FunctionCallValues vs -> return (Just vs)
      _                     -> return Nothing

-- | Call the given function. Blocks.
runFunction
  :: MonadJi m
  => String            -- ^ The function name.
  -> [String]          -- ^ Parameters.
  -> m ()
runFunction func params =
  run (CallFunction (func,params))

-- | Call the given function with the given continuation. Doesn't block.
callDeferredFunction
  :: MonadJi m
  => String                   -- ^ The function name.
  -> [String]                 -- ^ Parameters.
  -> ([Maybe String] -> m ()) -- ^ The continuation to call if/when the function completes.
  -> m ()
callDeferredFunction func params closure = do
  Session{..} <- askSession
  cid <- io $ modifyMVar sClosures (\(x:xs) -> return (xs,x))
  closure' <- newClosure func (show cid) closure
  run $ CallDeferredFunction (closure',func,params)
