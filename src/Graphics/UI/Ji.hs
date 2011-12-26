{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.UI.Ji
--  (serve)
  where
  
import Graphics.UI.Ji.Types

import           Control.Concurrent
import           Control.Monad.IO
import           Control.Monad.Reader
import           Data.ByteString      (ByteString)
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Monoid.Operator
import           Data.Text            (pack,unpack)
import           Data.Text.Encoding
import           Prelude              hiding ((++),init)
import           Safe
import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe
import           Text.JSON.Generic
import           Text.Printf

serve :: Int -> (Session -> IO a) -> IO ()
serve port worker = do
  sessions :: MVar (Map Integer Session) <- newMVar M.empty
  httpServe server (router worker sessions)
 where server = setPort port defaultConfig
 
runJi session m = runReaderT m session

router :: (Session -> IO a) -> MVar (Map Integer Session) -> Snap ()
router worker sessions = route routes where
  routes = [("/",handle)
           ,("/js/",serveDirectory "wwwroot/js")
           ,("/init",init worker sessions)
           ,("/poll",poll sessions)
           ,("/signal",signal sessions)]

handle :: Snap ()
handle = do
  writeText "<script src=\"/js/jquery.js\"></script>"
  writeText "<script src=\"/js/x.js\"></script>"

poll :: MVar (Map Integer Session) -> Snap ()
poll sessions = do
  withSession sessions $ \Session{..} -> do
    instructions <- io $ readAvailableChan sInstructions
    writeJson instructions

readAvailableChan :: Chan a -> IO [a]
readAvailableChan chan = do
  v <- readChan chan
  vs <- rest
  return (v:vs)

  where
    rest = do
      empty <- isEmptyChan chan
      if empty
         then return []
         else do v <- readChan chan
                 vs <- rest
                 return (v:vs)

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

init :: (Session -> IO void) -> MVar (Map Integer Session) -> Snap ()
init sessionThread sessions = do
  key <- io $ modifyMVar sessions $ \sessions -> do
    let newKey = maybe 0 (+1) (lastMay (M.keys sessions))
    session <- newSession newKey
    forkIO $ do _ <- sessionThread session; return ()
    return (M.insert newKey session sessions,newKey)
  writeJson $ SetToken key

handleEvents = forever $ do
  signal <- get
  case signal of
    ev@(Event (elid,eventType)) -> do
      io $ printf "Received event: %s\n" (show ev)
      Session{..} <- ask
      handlers <- io $ withMVar sEventHandlers return
      case M.lookup (elid,eventType) handlers of
        Nothing -> return ()
        Just handler -> handler EventData
    unknown -> return ()
 
onClick :: MonadJi m => Element -> EventHandler -> m ()
onClick = bind "click"

onHover :: MonadJi m => Element -> EventHandler -> m ()
onHover = bind "mouseenter"

onBlur :: MonadJi m => Element -> EventHandler -> m ()
onBlur = bind "mouseleave"

bind :: MonadJi m => String -> Element -> EventHandler -> m ()
bind eventType el handler = do
  closure <- newEventHandler eventType el handler
  run $ Bind eventType el closure

newEventHandler :: MonadJi m => String -> Element -> EventHandler -> m Closure
newEventHandler eventType (Element elid) thunk = do
  Session{..} <- askSession
  io $ modifyMVar_ sEventHandlers $ \handlers -> do
    return (M.insert key thunk  handlers)
  return (Closure key)
    
  where key = (elid,eventType)

setStyle el props = run $ SetStyle el props
setAttr el key value = run $ SetAttr el key value

setText el props = run $ SetText el props
setHtml el props = run $ SetHtml el props

append el props = run $ Append el props

getElementByTagName :: MonadJi m => String -> m (Maybe Element)
getElementByTagName = liftM listToMaybe . getElementsByTagName

getElementsByTagName :: MonadJi m => String -> m [Element]
getElementsByTagName tagName =
  call (GetElementsByTagName tagName) $ \signal ->
    case signal of
      Elements els -> return (Just els)
      _            -> return Nothing

getValue :: MonadJi m => Element -> m String
getValue el =
  call (GetValue el) $ \signal ->
    case signal of
      Value str -> return (Just str)
      _         -> return Nothing

readValue :: (MonadJi m,Read a) => Element -> m (Maybe a)
readValue = liftM readMay . getValue

debug :: MonadJi m => String -> m ()
debug = run . Debug

clear :: MonadJi m => m ()
clear = run $ Clear ()

newElement :: MonadJi m => String -> m Element
newElement tagName = do
  Session{..} <- askSession
  elid <- io $ modifyMVar sElementIds $ \elids ->
    return (tail elids,"*" ++ show (head elids) ++ ":" ++ tagName)
  return (Element elid)

run :: MonadJi m => Instruction -> m ()
run i = do
  Session{..} <- askSession
  io $ printf "Writing instruction: %s\n" (show i)
  io $ writeChan sInstructions i

get = do
  Session{..} <- ask
  io $ readChan sSignals
   
call :: MonadJi m => Instruction -> (Signal -> m (Maybe a)) -> m a
call instruction withSignal = do
  Session{..} <- askSession
  io $ printf "Calling instruction as function on: %s\n" (show instruction)
  run $ instruction
  newChan <- liftIO $ dupChan sSignals
  go newChan

  where
    go newChan = do
      signal <- liftIO $ readChan newChan
      result <- withSignal signal
      case result of
        Just signal -> do io $ printf "Got response for %s\n" (show instruction)
                          return signal
        Nothing     -> go newChan

withSession sessions cont = do
  token <- readInput "token"
  case token of
    Nothing -> error $ "Invalid session token format."
    Just token -> do
      sessions <- io $ withMVar sessions return
      case M.lookup token sessions of
        Nothing -> error $ "Nonexistant token: " ++ show token
        Just session -> cont session

newSession token = do
  signals <- newChan
  instructions <- newChan
  handlers <- newMVar M.empty
  mutex <- newMVar ()
  ids <- newMVar [0..]
  return $ Session
    { sSignals = signals
    , sInstructions = instructions
    , sEventHandlers = handlers
    , sElementIds = ids
    , sToken = token
    }

readInput :: (MonadSnap f,Read a) => ByteString -> f (Maybe a)
readInput = fmap (>>= readMay) . getInput

jsonInput :: (Data a, MonadSnap f) => ByteString -> f (Maybe a)
jsonInput = fmap (fmap decodeJSON) . getInput

getInput :: (MonadSnap f) => ByteString -> f (Maybe String)
getInput = fmap (fmap (unpack . decodeUtf8)) . getParam

writeString :: (MonadSnap m) => String -> m ()
writeString = writeText . pack

writeJson :: (MonadSnap m, Data a) => a -> m ()
writeJson = writeString . encodeJSON
