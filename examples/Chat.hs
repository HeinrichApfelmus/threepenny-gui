{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO
import Data.Maybe
import Data.Time
import Graphics.UI.Ji
import Graphics.UI.Ji.DOM
import Graphics.UI.Ji.Elements
import Graphics.UI.Ji.JQuery
import Prelude                 hiding (catch)
import Text.JSON

type Msg = (UTCTime,String,String)

main :: IO ()
main = do
  messages <- newChan
  serve Config
    { jiPort = 10004
    , jiRun = runJi
    , jiWorker = worker messages
    , jiInitHTML = "chat.html"
    , jiStatic = "wwwroot"
    }

-- | The worker thread.
worker :: Chan Msg -> Ji ()
worker msgs = do
  msgs <- io $ dupChan msgs
  body <- getBody
  wrap <- new #. "wrap" #+ body
  nickname <- getNickname
  header <- new #. "header" #= "Ji Chat" #+ wrap
  new #. "gradient" #+ wrap
  codeLink wrap
  messageArea <- new #. "message-area" #+ wrap
  sendArea <- new #. "send-area" #+ wrap
  input <- newTextarea #. "send-textarea" #+ sendArea
  setFocus nickname
  onSendValue input $ \content -> do
    when (not (null content)) $ do
      now <- io $ getCurrentTime
      (trim -> nick) <- getValue nickname
      set "value" "" input
      when (not (null nick)) $ do
        io $ writeChan msgs (now,nick,content)
  session <- askSession
  messageReceiver <- io $ forkIO $ runJi session $ do
    messages <- io $ getChanContents msgs
    forM_ messages $ \ (time,user,content) -> do
      atomic $ newMessage time user content # addMessage messageArea # unit
  io $ catch (runJi session handleEvents)
             (\e -> do killThread messageReceiver
                       throw (e :: SomeException))

-- | Get the nickname as a little form.
getNickname :: Ji Element
getNickname = do
  body <- getBody
  myname <- new #. "name-area" #+ body
  newLabel #= "Your name " #. "name-label" #+ myname
  input <- newInput #+ myname #. "name-input"
  return input

-- | Make a new message.
newMessage :: UTCTime -> String -> String -> Ji Element
newMessage timestamp nick content = do
  msg <- new #. "message"
  new #. "timestamp" #= show timestamp #+ msg # unit
  new #. "name" #= nick ++ " says:" #+ msg # unit
  new #. "content" #= content #+ msg # unit
  return msg

-- | Add the given message to the message area.
addMessage :: Element -> Element -> Ji ()
addMessage area message = do
  addTo area message # unit
  runFunction "jquery_scrollToBottom" [encode area]

-- | Do something on return.
onSendValue :: (MonadJi m) => Element -> (String -> m ()) -> m ()
onSendValue input m = do
  bind "sendvalue" input $ \(EventData evdata) -> do
    m (concat (catMaybes evdata))

-- | Focus an element.
setFocus :: (MonadJi m) => Element -> m ()
setFocus el = runFunction "jquery_setFocus" [encode el]

-- | Simple trim.
trim :: String -> String
trim = filter (/='\n') . unwords . words

codeLink :: Element -> Ji ()
codeLink wrap = do
  newAnchor # set "href" "https://github.com/chrisdone/ji/blob/master/examples/Chat.hs"
            # setText "View source code"
            # setClass "code-link"
            # addTo wrap
            # unit
