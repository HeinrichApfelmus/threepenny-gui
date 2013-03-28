{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO
import Data.List.Extra
import Data.Time
import Graphics.UI.Ji
import Graphics.UI.Ji.Browser
import Prelude hiding (catch)

main = do
  messages <- newChan
  serve $ Config 10004 runJi (worker messages) "chat.html" "wwwroot"

worker globalMsgs = do
  body <- getBody
  new #. "header" #= "Ji Chat" #+ body # unit
  new #. "gradient" #+ body # unit
  codeLink body
  nickname <- getNickname body
  messageArea <- new #. "message-area" #+ body
  msgs <- io $ dupChan globalMsgs
  sendMessageArea body nickname msgs
  messageReceiver <- receiveMessages msgs messageArea
  session <- askSession
  io $ catch (runJi session handleEvents)
             (\e -> do killThread messageReceiver
                       throw (e :: SomeException))

receiveMessages msgs messageArea = forkJi $ do
  messages <- io $ getChanContents msgs
  forM_ messages $ \ (time,user,content) -> do
    atomic $ do
      newMessage time user content # addTo messageArea # unit
      scrollToBottom messageArea

sendMessageArea parent nickname msgs = do
  sendArea <- new #. "send-area" #+ parent
  input <- newTextarea #. "send-textarea" #+ sendArea
  onSendValue input $ \(trim -> content) -> do
    when (not (null content)) $ do
      now <- io $ getCurrentTime
      (trim -> nick) <- getValue nickname
      set "value" "" input # unit
      when (not (null nick)) $ do
        io $ writeChan msgs (now,nick,content)

getNickname parent = do
  myname <- new #. "name-area" #+ parent
  newLabel #= "Your name " #. "name-label" #+ myname # unit
  input <- newInput #+ myname #. "name-input" # setFocus
  return input

newMessage timestamp nick content = do
  msg <- new #. "message"
  new #. "timestamp" #= show timestamp #+ msg # unit
  new #. "name" #= nick ++ " says:" #+ msg # unit
  new #. "content" #= content #+ msg # unit
  return msg

codeLink parent = do
  newAnchor # set "href" url # setText label # setClass "code-link" # addTo parent # unit
  where url = "https://github.com/chrisdone/ji/blob/master/examples/Chat.hs"
        label = "View source code"
