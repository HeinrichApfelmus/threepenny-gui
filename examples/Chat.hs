{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO
import Data.List.Extra
import Data.Map                (Map)
import qualified Data.Map      as M
import Data.Maybe
import Data.Time
import Graphics.UI.Ji
import Graphics.UI.Ji.DOM
import Graphics.UI.Ji.Elements
import Graphics.UI.Ji.JQuery
import Prelude hiding (catch)

main = do
  nicks <- newMVar M.empty
  messages <- newChan
  serve Config
    { jiPort = 10004
    , jiRun = runJi
    , jiWorker = worker messages nicks
    , jiInitHTML = "chat.html"
    , jiStatic = "wwwroot"
    }

-- Start thread.
worker globalMsgs nicks = do
  body <- getBody
  addHeader
  codeLink
  nicklistUpdater <- nicklistDisplay nicks
  nickname        <- getNickname nicks
  messageArea     <- new #. "message-area" #+ body
  msgs <- io $ dupChan globalMsgs
  sendMessageArea nickname msgs
  session <- askSession
  messageReceiver <- receiveMessages msgs messageArea
  io $ catch (runJi session handleEvents)
             (\e -> do killThread messageReceiver
                       killThread nicklistUpdater
                       modifyMVar_ nicks $ return . M.delete (sToken session)
                       throw (e :: SomeException))
                       
  where addHeader = do
          body <- getBody
          new #. "header" #= "Ji Chat" #+ body # unit
          new #. "gradient" #+ body # unit

-- Receive messages from users (including myself).
receiveMessages msgs messageArea = forkJi $ do
  messages <- io $ getChanContents msgs
  forM_ messages $ \ (time,user,content) -> do
    atomic $ newMessage time user content # addMessage messageArea # unit

-- Send message box, writes to the messages channel.
sendMessageArea nickname msgs = do
  body <- getBody
  sendArea <- new #. "send-area" #+ body
  input <- newTextarea #. "send-textarea" #+ sendArea
  setFocus nickname
  onSendValue input $ \(trim -> content) -> do
    when (not (null content)) $ do
      now <- io $ getCurrentTime
      (trim -> nick) <- getValue nickname
      set "value" "" input # unit
      when (not (null nick)) $ do
        io $ writeChan msgs (now,nick,content)

-- Get the nickname as a little form.
getNickname nicks = do
  body <- getBody
  myname <- new #. "name-area" #+ body
  newLabel #= "Your name " #. "name-label" #+ myname # unit
  input <- newInput #+ myname #. "name-input"
  session <- askSession
  bind "livechange" input $ \(EventData (concat . catMaybes -> nick)) -> do
    io $ modifyMVar_ nicks $ return . (M.insert (sToken session) nick)
  return input

-- Display the nicks.
nicklistDisplay nicks = do
  body <- getBody
  nicklist <- new #. "nicklist" #+ body
  displayer <- forkJi $ do
    flip fix [] $ \loop prev -> do
      io $ threadDelay $ 1000 * 3000
      nicknames <- liftM M.elems $ io $ readMVar nicks
      when (nicknames /= prev) $ do
        emptyEl nicklist # unit
        forM_ nicknames $ \nick -> do
          new #. "nick" #+ nicklist #= nick
      loop nicknames
  return displayer

-- Make a new message.
newMessage timestamp nick content = do
  msg <- new #. "message"
  new #. "timestamp" #= show timestamp #+ msg # unit
  new #. "name" #= nick ++ " says:" #+ msg # unit
  new #. "content" #= content #+ msg # unit
  return msg

-- Add the given message to the message area.
addMessage area message = do
  addTo area message # unit
  scrollToBottom area

-- Link to the site's source.
codeLink = do
  body <- getBody
  newAnchor # set "href" "https://github.com/chrisdone/ji/blob/master/examples/Chat.hs"
            # setText "View source code"
            # setClass "code-link"
            # addTo body
            # unit
