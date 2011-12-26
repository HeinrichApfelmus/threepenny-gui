-- | The main Ji module.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Graphics.UI.Ji
  (serve
  ,runJi
  ,onClick
  ,onHover
  ,onBlur
  ,bind
  ,newEventHandler
  ,setStyle
  ,setAttr
  ,setText
  ,setHtml
  ,append
  ,getElementsByTagName
  ,getElementByTagName
  ,getValue
  ,getValuesList
  ,readValue
  ,readValuesList
  ,debug
  ,clear
  ,newElement
  ,handleEvents)
  where
  
import           Graphics.UI.Ji.Types

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
import           Text.Printf

--------------------------------------------------------------------------------
-- Exported

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
  writeText "<script src=\"/js/jquery.js\"></script>"
  writeText "<script src=\"/js/x.js\"></script>"

-- Initialize the session.
init :: (Session m -> IO void) -> MVar (Map Integer (Session m)) -> Snap ()
init sessionThread sessions = do
  key <- io $ modifyMVar sessions $ \sessions -> do
    let newKey = maybe 0 (+1) (lastMay (M.keys sessions))
    session <- newSession newKey
    _ <- forkIO $ do _ <- sessionThread session; return ()
    return (M.insert newKey session sessions,newKey)
  writeJson $ SetToken key

-- Respond to poll requests.
poll :: MVar (Map Integer (Session m)) -> Snap ()
poll sessions = do
  withSession sessions $ \Session{..} -> do
    instructions <- io $ readAvailableChan sInstructions
    writeJson instructions

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

-- Handle events signalled from the client.
handleEvents :: MonadJi m => m ()
handleEvents = forever $ do
  signal <- get
  case signal of
    ev@(Event (elid,eventType)) -> do
      _ <- io $ printf "Received event: %s\n" (show ev)
      Session{..} <- askSession
      handlers <- io $ withMVar sEventHandlers return
      case M.lookup (elid,eventType) handlers of
        Nothing -> return ()
        Just handler -> handler EventData
    _ -> return ()

-- | Bind an event handler to the click event of the given element.
onClick :: MonadJi m => Element -> (EventData -> m ()) -> m ()
onClick = bind "click"

-- | Bind an event handler to the hover event of the given element.
onHover :: MonadJi m => Element -> (EventData -> m ()) -> m ()
onHover = bind "mouseenter"

-- | Bind an event handler to the blur event of the given element.
onBlur :: MonadJi m => Element -> (EventData -> m ()) -> m ()
onBlur = bind "mouseleave"

-- | Bind an event handler to the given event of the given element.
bind :: MonadJi m => String -> Element -> (EventData -> m ()) -> m ()
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

-- | Set the style of the given element.
setStyle :: (MonadJi m) => Element -> [(String, String)] -> m ()
setStyle el props = run $ SetStyle el props

-- | Set the attribute of the given element.
setAttr :: (MonadJi m) => Element -> String -> String -> m ()
setAttr el key value = run $ SetAttr el key value

-- | Set the text of the given element.
setText :: (MonadJi m) => Element -> String -> m ()
setText el props = run $ SetText el props

-- | Set the HTML of the given element.
setHtml :: (MonadJi m) => Element -> String -> m ()
setHtml el props = run $ SetHtml el props

-- | Append one element to another.
append :: (MonadJi m) => Element -> Element -> m ()
append el props = run $ Append el props

-- | Get an element by its tag name.
getElementByTagName :: MonadJi m => String -> m (Maybe Element)
getElementByTagName = liftM listToMaybe . getElementsByTagName

-- | Get all elements of the given tag name.
getElementsByTagName :: MonadJi m => String -> m [Element]
getElementsByTagName tagName =
  call (GetElementsByTagName tagName) $ \signal ->
    case signal of
      Elements els -> return (Just els)
      _            -> return Nothing

-- | Get the value of an input.
getValue :: MonadJi m => Element -> m String
getValue el =
  call (GetValue el) $ \signal ->
    case signal of
      Value str -> return (Just str)
      _         -> return Nothing

-- | Get values from inputs.
getValuesList :: MonadJi m => [Element] -> m [String]
getValuesList el =
  call (GetValues el) $ \signal ->
    case signal of
      Values strs -> return (Just strs)
      _           -> return Nothing

-- | Read a value from an input.
readValue :: (MonadJi m,Read a) => Element -> m (Maybe a)
readValue = liftM readMay . getValue

-- | Read values from inputs.
readValuesList :: (MonadJi m,Read a) => [Element] -> m (Maybe [a])
readValuesList = liftM (sequence . map readMay) . getValuesList

-- | Send a debug message to the client.
debug :: MonadJi m => String -> m ()
debug = run . Debug

-- | Clear the client's DOM.
clear :: MonadJi m => m ()
clear = run $ Clear ()

-- | Create a new element of the given tag name.
newElement :: MonadJi m => String -> m Element
newElement tagName = do
  Session{..} <- askSession
  elid <- io $ modifyMVar sElementIds $ \elids ->
    return (tail elids,"*" ++ show (head elids) ++ ":" ++ tagName)
  return (Element elid)

-- Run the given instruction.
run :: MonadJi m => Instruction -> m ()
run i = do
  Session{..} <- askSession
  _ <- io $ printf "Writing instruction: %s\n" (show i)
  io $ writeChan sInstructions i

-- Get the latest signal sent from the client.
get :: MonadJi m => m Signal
get = do
  Session{..} <- askSession
  io $ readChan sSignals
   
-- Send an instruction and read the signal response.
call :: MonadJi m => Instruction -> (Signal -> m (Maybe a)) -> m a
call instruction withSignal = do
  Session{..} <- askSession
  _ <- io $ printf "Calling instruction as function on: %s\n" (show instruction)
  run $ instruction
  newChan <- liftIO $ dupChan sSignals
  go newChan

  where
    go newChan = do
      signal <- liftIO $ readChan newChan
      result <- withSignal signal
      case result of
        Just signal -> do _ <- io $ printf "Got response for %s\n" (show instruction)
                          return signal
        Nothing     -> go newChan

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

-- Read an input from snap.
readInput :: (MonadSnap f,Read a) => ByteString -> f (Maybe a)
readInput = fmap (>>= readMay) . getInput

-- Get a text input from snap.
getInput :: (MonadSnap f) => ByteString -> f (Maybe String)
getInput = fmap (fmap (unpack . decodeUtf8)) . getParam

-- Write a string to output.
writeString :: (MonadSnap m) => String -> m ()
writeString = writeText . pack

-- Write JSON to output.
writeJson :: (MonadSnap m, Data a) => a -> m ()
writeJson = writeString . encodeJSON
