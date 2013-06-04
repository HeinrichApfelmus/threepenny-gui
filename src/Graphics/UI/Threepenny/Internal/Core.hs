{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

-- | The main Threepenny module.


--------------------------------------------------------------------------------
-- Exports

module Graphics.UI.Threepenny.Internal.Core
  (
  -- * Server running
   serve
  
  -- * Event handling
  -- $eventhandling
  ,bind
  ,handleEvent
  ,handleEvents
  ,module Control.Event
  
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
  ,appendElementTo
  
  -- * Querying
  -- $querying
  ,getHead
  ,getBody
  ,getElementsByTagName
  ,getElementByTagName
  ,getElementsById
  ,getElementById
  ,getValue
  ,getValuesList
  ,readValue
  ,readValuesList
  ,getRequestCookies
  ,getRequestLocation
  
  -- * Utilities
  ,debug
  ,clear
  ,callFunction
  ,runFunction
  ,callDeferredFunction
  ,atomic
  
  -- * Types
  ,Window
  ,Element
  ,Config(..)
  ,EventData(..)
  ) where


--------------------------------------------------------------------------------
-- Imports

import           Graphics.UI.Threepenny.Internal.Types     as Threepenny
import           Graphics.UI.Threepenny.Internal.Resources

import           Control.Concurrent
import           Control.Concurrent.Chan.Extra
import           Control.Concurrent.Delay
import qualified Control.Exception             as E
import           Control.Event
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
-- Server running

-- | Run a TP server with Snap on the specified port and the given
--   worker action.
serve :: Config -> (Session -> IO ()) -> IO () -- ^ A TP server.
serve Config{..} worker = do
  sessions <- newMVar M.empty
  _ <- forkIO $ custodian 30 sessions
  httpServe server (router tpCustomHTML tpStatic worker sessions)
 where server = setPort tpPort defaultConfig

-- | Kill sessions after at least n seconds of disconnectedness.
custodian :: Integer -> MVar (Map Integer Session) -> IO ()
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

-- Route requests.  If the initFile is Nothing, then a default
-- file will be served at /.
router
    :: Maybe FilePath
    -> FilePath
    -> (Session -> IO a)
    -> MVar (Map Integer Session)
    -> Snap ()
router customHTML wwwroot worker sessions =
        route [("/static"                    , serveDirectory wwwroot)
              ,("/"                          , root)
              ,("/driver/threepenny-gui.js"  , writeText jsDriverCode )
              ,("/driver/threepenny-gui.css" , writeText cssDriverCode)
              ,("/init"                      , init worker sessions)
              ,("/poll"                      , poll sessions)
              ,("/signal"                    , signal sessions)
              ]
    where
    root = case customHTML of
        Just file -> serveFile (wwwroot </> file)
        Nothing   -> writeText defaultHtmlFile


-- Initialize the session.
init :: (Session -> IO void) -> MVar (Map Integer Session) -> Snap ()
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
newSession :: (URI,[(String, String)]) -> Integer -> IO Session
newSession info token = do
  signals <- newChan
  instructions <- newChan
  (event, handler) <- newEventsTagged
  ids <- newMVar [0..]
  mutex <- newMVar ()
  now <- getCurrentTime
  conState <- newMVar (Disconnected now)
  threadId <- myThreadId
  closures <- newMVar [0..]
  return $ Session
    { sSignals = signals
    , sInstructions = instructions
    , sEvent        = event
    , sEventHandler = handler
    , sElementIds = ids
    , sToken = token
    , sMutex = mutex
    , sConnectedState = conState
    , sThreadId = threadId
    , sClosures = closures
    , sStartInfo = info
    }

-- Respond to poll requests.
poll :: MVar (Map Integer Session) -> Snap ()
poll sessions = withSession sessions pollWithSession
  
-- Poll for the given session.
pollWithSession :: Session -> Snap ()
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
signal :: MVar (Map Integer Session) -> Snap ()
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
handleEvents :: Window -> IO ()
handleEvents window = forever $ handleEvent window

-- | Handle one event.
handleEvent :: Window -> IO ()
handleEvent window@(Session{..}) = do
    signal <- getSignal window
    case signal of
        Threepenny.Event (elid,eventType,params) -> do
            sEventHandler ((elid,eventType), EventData params)
        _ -> return ()

-- Get the latest signal sent from the client.
getSignal :: Window -> IO Signal
getSignal (Session{..}) = readChan sSignals

-- | Return an 'Event' associated to an 'Element'.
bind
    :: String               -- ^ The eventType, see any DOM documentation for a list of these.
    -> Element              -- ^ The element to bind to.
    -> Event EventData      -- ^ The event handler.
bind eventType (Element el@(ElementId elid) session) =
    Control.Event.Event register
    where
    key        = (elid, eventType)
    register h = do
        -- register with server
        unregister <- Control.Event.register (sEvent session key) h
        -- register with client
        run session $ Bind eventType el (Closure key)
        return unregister

-- Make a uniquely numbered event handler.
newClosure :: Window -> String -> String -> ([Maybe String] -> IO ()) -> IO Closure
newClosure window@(Session{..}) eventType elid thunk = do
    let key = (elid, eventType)
    _ <- register (sEvent key) $ \(EventData xs) -> thunk xs
    return (Closure key)

--------------------------------------------------------------------------------
-- Setting attributes
--
-- $settingattributes
-- 
-- Text, HTML and attributes of DOM nodes can be set using the
-- functions in this section. 

-- | Set the style of the given element.
setStyle :: [(String, String)] -- ^ Pairs of CSS (property,value).
         -> Element            -- ^ The element to update.
         -> IO Element
setStyle props e@(Element el session) = do
    run session $ SetStyle el props
    return e

-- | Set the attribute of the given element.
setAttr :: String  -- ^ The attribute name.
        -> String  -- ^ The attribute value.
        -> Element -- ^ The element to update.
        -> IO Element
setAttr key value e@(Element el session) = do
    run session $ SetAttr el key value
    return e

-- | Set the text of the given element.
setText :: String  -- ^ The plain text.
        -> Element -- ^ The element to update.
        -> IO Element
setText props e@(Element el session) = do
    run session $ SetText el props
    return e

-- | Set the HTML of the given element.
setHtml :: String  -- ^ The HTML.
        -> Element -- ^ The element to update.
        -> IO Element
setHtml props e@(Element el session) = do
    run session $ SetHtml el props
    return e

-- | Set the title of the document.
setTitle
    :: String  -- ^ The title.
    -> Window  -- ^ The document window
    -> IO ()
setTitle title session = run session $ SetTitle title

-- | Empty the given element.
emptyEl :: Element -> IO Element
emptyEl e@(Element el session) = do
    run session $ EmptyEl el
    return e

-- | Delete the given element.
delete :: Element -> IO ()
delete e@(Element el session) = do
    run session $ Delete el


--------------------------------------------------------------------------------
-- Manipulating tree structure
--
-- $treestructure
-- 
-- Functions for creating, deleting, moving, appending, prepending, DOM nodes.

-- | Create a new element of the given tag name.
newElement :: Window      -- ^ Browser window in which context to create the element
           -> String      -- ^ The tag name.
           -> IO Element  -- ^ A tag reference. Non-blocking.
newElement session@(Session{..}) tagName = do
    -- TODO: Remove the need to specify in which browser window is to be created
    elid <- modifyMVar sElementIds $ \elids ->
        return (tail elids,"*" ++ show (head elids) ++ ":" ++ tagName)
    return (Element (ElementId elid) session)

-- | Append a child element to a parent element. Non-blocking.
appendElementTo
    :: Element     -- ^ Parent.
    -> Element     -- ^ Child.
    -> IO Element  -- ^ Returns a reference to the child element again.
appendElementTo (Element parent session) e@(Element child _) = do
    -- TODO: Right now, parent and child need to be from the same session/browser window
    --       Implement transfer of elements across browser windows
    run session $ Append parent child
    return e


--------------------------------------------------------------------------------
-- Querying the DOM
--
-- $querying
-- 
-- The DOM can be searched for elements of a given name, and nodes can
-- be inspected for their values.

-- | Get an element by its tag name.  Blocks.
getElementByTagName
    :: Window             -- ^ Browser window
    -> String             -- ^ The tag name.
    -> IO (Maybe Element) -- ^ An element (if any) with that tag name.
getElementByTagName window = liftM listToMaybe . getElementsByTagName window

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

-- | Get an element by a particular ID.  Blocks.
getElementById
    :: Window              -- ^ Browser window
    -> String              -- ^ The ID string.
    -> IO (Maybe Element)  -- ^ Element (if any) with given ID.
getElementById window id = listToMaybe `fmap` getElementsById window [id]

-- | Get the value of an input. Blocks.
getValue
    :: Element   -- ^ The element to get the value of.
    -> IO String -- ^ The plain text value.
getValue e@(Element el window) =
  call window (GetValue el) $ \signal ->
    case signal of
      Value str -> return (Just str)
      _         -> return Nothing

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

-- | Atomically execute the given computation in the context of a browser window
atomic :: Window -> IO a -> IO a
atomic window@(Session{..}) m = do
  takeMVar sMutex
  ret <- m
  putMVar sMutex ()
  return ret


-- Send an instruction and read the signal response.
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

-- Run the given instruction.
run :: Session -> Instruction -> IO ()
run (Session{..}) i = writeChan sInstructions i

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


--------------------------------------------------------------------------------
-- Utilities

-- | Send a debug message to the client. The behaviour of the client
--   is unspecified.
debug
    :: Window -- ^ Client window
    -> String -- ^ Some plain text to send to the client.
    -> IO ()
debug window = run window . Debug

-- | Clear the client's DOM.
clear :: Window -> IO ()
clear window = run window $ Clear ()

-- | Call the given function and wait for results. Blocks.
callFunction
  :: Window             -- ^ Browser window
  -> String             -- ^ The function name.
  -> [String]           -- ^ Parameters.
  -> IO [Maybe String]  -- ^ Return tuple.
callFunction window func params =
  call window (CallFunction (func,params)) $ \signal ->
    case signal of
      FunctionCallValues vs -> return (Just vs)
      _                     -> return Nothing

-- | Call the given function and carry on. Doesn't block.
runFunction
  :: Window            -- ^ Browser window
  -> String            -- ^ The function name.
  -> [String]          -- ^ Parameters.
  -> IO ()
runFunction window func params =
  run window $ CallFunction (func,params)

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
