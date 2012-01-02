-- | The main Ji module.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing #-}


--------------------------------------------------------------------------------
-- Exports

module Graphics.UI.Ji
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
  
  -- * Manipulating tree structure
  -- $treestructure
  ,newElement
  ,append
  
  -- * Querying
  -- $querying
  ,getElementsByTagName
  ,getElementByTagName
  ,getValue
  ,getValuesList
  ,readValue
  ,readValuesList
  
  -- * Utilities
  -- $utilities
  ,debug
  ,clear
  
  -- * Types
  ,module Graphics.UI.Ji.Types)
  where


--------------------------------------------------------------------------------
-- Imports

import           Graphics.UI.Ji.Types
import           Graphics.UI.Ji.Internal.Types

import           Control.Concurrent
import           Control.Concurrent.Chan.Extra
import           Control.Monad.IO
import           Control.Monad.Reader
import           Data.ByteString               (ByteString)
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Monoid.Operator
import           Data.Text                     (pack,unpack)
import           Data.Text.Encoding
import           Prelude                       hiding ((++),init)
import           Safe
import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe
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
serve :: MonadJi m
      => Int                        -- ^ Port.
      -> (Session m -> m a -> IO a) -- ^ How to run the worker monad.
      -> m a                        -- ^ The worker.
      -> IO ()                      -- ^ A Ji server.
serve port run worker = do
  sessions <- newMVar M.empty
  httpServe server (router (\session -> run session worker) sessions)
 where server = setPort port defaultConfig

-- | Convenient way to a run a worker with a session.
runJi :: Session Ji  -- ^ The browser session.
      -> Ji a       -- ^ The worker.
      -> IO a       -- ^ A worker runner.
runJi session m = runReaderT (getJi m) session

-- Route requests.
router :: (Session m -> IO a) -> MVar (Map Integer (Session m)) -> Snap ()
router worker sessions = route routes where
  routes = [("/",handle)
           ,("/js/",serveDirectory "wwwroot/js")
           ,("/init",init worker sessions)
           ,("/poll",poll sessions)
           ,("/signal",signal sessions)]

-- Setup the JS.
handle :: Snap ()
handle = do
  writeText "<script src=\"js/jquery.js\"></script>"
  writeText "<script src=\"js/x.js\"></script>"

-- Initialize the session.
init :: (Session m -> IO void) -> MVar (Map Integer (Session m)) -> Snap ()
init sessionThread sessions = do
  key <- io $ modifyMVar sessions $ \sessions -> do
    let newKey = maybe 0 (+1) (lastMay (M.keys sessions))
    session <- newSession newKey
    _ <- forkIO $ do _ <- sessionThread session; return ()
    return (M.insert newKey session sessions,newKey)
  writeJson $ SetToken key

-- Make a new session.
newSession :: Integer -> IO (Session m)
newSession token = do
  signals <- newChan
  instructions <- newChan
  handlers <- newMVar M.empty
  ids <- newMVar [0..]
  return $ Session
    { sSignals = signals
    , sInstructions = instructions
    , sEventHandlers = handlers
    , sElementIds = ids
    , sToken = token
    }

-- Respond to poll requests.
poll :: MVar (Map Integer (Session m)) -> Snap ()
poll sessions = do
  withSession sessions $ \Session{..} -> do
    instructions <- io $ readAvailableChan sInstructions
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
    Just token -> do
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
    Event (elid,eventType) -> do
      Session{..} <- askSession
      handlers <- io $ withMVar sEventHandlers return
      case M.lookup (elid,eventType) handlers of
        Nothing -> return ()
        Just handler -> handler EventData
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
bind eventType el handler = do
  closure <- newEventHandler eventType el handler
  run $ Bind eventType el closure

-- Make a uniquely numbered event handler.
newEventHandler :: MonadJi m => String -> Element -> (EventData -> m ()) -> m Closure
newEventHandler eventType (Element elid) thunk = do
  Session{..} <- askSession
  io $ modifyMVar_ sEventHandlers $ \handlers -> do
    return (M.insert key thunk  handlers)
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
         => Element            -- ^ The element to update.
         -> [(String, String)] -- ^ Pairs of CSS (property,value).
         -> m ()
setStyle el props = run $ SetStyle el props

-- | Set the attribute of the given element.
setAttr :: (MonadJi m)
        => Element -- ^ The element to update.
        -> String  -- ^ The attribute name.
        -> String  -- ^ The attribute value.
        -> m ()
setAttr el key value = run $ SetAttr el key value

-- | Set the text of the given element.
setText :: (MonadJi m)
        => Element -- ^ The element to update.
        -> String  -- ^ The plain text.
        -> m ()
setText el props = run $ SetText el props

-- | Set the HTML of the given element.
setHtml :: (MonadJi m)
        => Element -- ^ The element to update.
        -> String  -- ^ The HTML.
        -> m ()
setHtml el props = run $ SetHtml el props


-- | Set the title of the document.
setTitle :: (MonadJi m)
        => String  -- ^ The title.
        -> m ()
setTitle title = run $ SetTitle title


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
append :: (MonadJi m)
       => Element -- ^ The resulting parent.
       -> Element -- ^ The resulting child.
       -> m ()
append el props = run $ Append el props


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
   
-- Send an instruction and read the signal response.
call :: MonadJi m => Instruction -> (Signal -> m (Maybe a)) -> m a
call instruction withSignal = do
  Session{..} <- askSession
  run $ instruction
  newChan <- liftIO $ dupChan sSignals
  go newChan

  where
    go newChan = do
      signal <- liftIO $ readChan newChan
      result <- withSignal signal
      case result of
        Just signal -> return signal
        Nothing     -> go newChan

-- Run the given instruction.
run :: MonadJi m => Instruction -> m ()
run i = do
  Session{..} <- askSession
  io $ writeChan sInstructions i


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
