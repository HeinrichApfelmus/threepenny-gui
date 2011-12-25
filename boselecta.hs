{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad.IO
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Data.ByteString         (ByteString)
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Maybe
import           Data.Monoid.Operator
import           Data.Text               (pack,unpack)
import           Data.Text.Encoding
import           Prelude                 hiding ((++),init)
import           Safe
import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe
import           Text.JSON.Generic
import           Text.Printf

data Instruction
  = Debug String
  | Begin ()
  | End ()
  | SetToken Integer
  | Clear ()
  | GetElementById String
  | GetElementsByTagName String
  | SetStyle Element [(String,String)]
  | NewElement String
  | Append Element Element
  | SetText Element String
  | SetHtml Element String
  | Bind String Element Closure
  deriving (Typeable,Data,Show)

data Signal
  = Init ()
  | Elements [Element]
  | SingleElement Element
  | Event (String,String)
  deriving (Show)

data Element = Element String
  deriving (Data,Typeable,Show)

instance JSON Signal where
  readJSON obj = do
    obj <- readJSON obj
    let init = Init <$> valFromObj "Init" obj
        elements = Elements <$> valFromObj "Elements" obj
        element = SingleElement <$> valFromObj "SingleElement" obj
        event = Event <$> valFromObj "Event" obj
    init <|> elements <|> element <|> event
  
instance JSON Element where
  readJSON obj = do
    obj <- readJSON obj
    Element <$> valFromObj "Element" obj

data Session = Session
  { sSignals :: Chan Signal
  , sInstructions :: Chan Instruction
  , sEventHandlers :: MVar (Map (String,String) EventHandler)
  , sElementIds :: MVar [Integer]
  }

main = do
  sessions :: MVar (Map Integer Session) <- newMVar M.empty
  httpServe server (serve sessions)
 where server = setPort 10001 defaultConfig

serve sessions = route routes where
  routes = [("/",handle)
           ,("/js/",serveDirectory "wwwroot/js")
           ,("/init",init sessions)
           ,("/poll",poll sessions)
           ,("/signal",signal sessions)]

handle = do
  writeText "<script src=\"/js/jquery.js\"></script>"
  writeText "<script src=\"/js/x.js\"></script>"

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

init sessions = do
  key <- io $ modifyMVar sessions $ \sessions -> do
    let newKey = maybe 0 (+1) (lastMay (M.keys sessions))
    session <- newSession
    forkIO $ sessionThread newKey session
    return (M.insert newKey session sessions,newKey)
  writeJson $ SetToken key

sessionThread token session@Session{..} = flip runReaderT session $ do
  initSession
  els <- getElementByTagName "body"
  case els of
    Nothing -> error "Where's the body?"
    Just body -> do
      debug $ "Got body: " ++ show body
      setStyle body [("background","#333")]
      greet body
      addButton body
  handleEvents
  
  where 
 
       initSession = do
         signal <- get
         case signal of
           Init () -> do
             debug "Init OK."
             debug "Clearing screen…"
             clear
           signal  -> error $ "Expected signal at start-up time: " ++ show signal

       greet body = do
         greeting <- newElement "div"
         append body greeting
         setText greeting "Hello, Haskell!"
         setStyle greeting [("color","#ccc")]
         
       addButton body = do
         list <- newElement "ul"
         button <- appendButton body "Button1"
         button2 <- appendButton body "Button2"
         setStyle list [("color","#aaa")]
         append body list
         onHover button $ \event -> do
           setText button "Button1[hover]"
         onHover button2 $ \event -> do
           setText button2 "Button2[hover]"
         onClick button $ \event -> do
           li <- newElement "li"
           io $ threadDelay $ 1000 * 1000 * 1
           setText button "Button1[pressed]"
           setHtml li "W<b>o</b><u>o</u>p!"
           append list li
         onClick button2 $ \event -> do
           li <- newElement "li"
           setText button2 "Button2[pressed]"
           setHtml li "Zap!"
           append list li

       appendButton body caption = do
         button <- newElement "a"
         append body button
         setText button caption
         setStyle button [("cursor","pointer")
                         ,("color","#acc2a1")
                         ,("text-decoration","underline")]
         return button

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
 
onClick = bind "click"
onHover = bind "hover"

data EventData = EventData

data Closure = Closure (String,String)
  deriving (Typeable,Data,Show)

type EventHandler = EventData -> SessionM ()

type SessionM a = ReaderT Session IO a

bind :: String -> Element -> EventHandler -> SessionM ()
bind eventType el handler = do
  closure <- newEventHandler eventType el handler
  run $ Bind eventType el closure

newEventHandler :: String -> Element -> EventHandler -> SessionM Closure
newEventHandler eventType (Element elid) thunk = do
  Session{..} <- ask
  io $ modifyMVar_ sEventHandlers $ \handlers -> do
    return (M.insert key thunk  handlers)
  return (Closure key)
    
  where key = (elid,eventType)

setStyle el props = run $ SetStyle el props

setText el props = run $ SetText el props
setHtml el props = run $ SetHtml el props

append el props = run $ Append el props

getElementByTagName = fmap listToMaybe . getElementsByTagName

getElementsByTagName tagName =
  call (GetElementsByTagName tagName) $ \signal ->
    case signal of
      Elements els -> return (Just els)
      _            -> return Nothing

debug = run . Debug

clear = run $ Clear ()

-- newElement tagName = 
--   call (NewElement tagName) $ \signal ->
--     case signal of
--       SingleElement el -> return (Just el)
--       _                -> return Nothing

newElement tagName = do
  Session{..} <- ask
  elid <- io $ modifyMVar sElementIds $ \elids ->
    return (tail elids,"*" ++ show (head elids) ++ ":" ++ tagName)
  return (Element elid)

run i = do
  Session{..} <- ask
  io $ printf "Writing instruction: %s\n" (show i)
  io $ writeChan sInstructions i

get = do
  Session{..} <- ask
  io $ readChan sSignals
   
call instruction withSignal = do
  Session{..} <- ask
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

newSession = do
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
