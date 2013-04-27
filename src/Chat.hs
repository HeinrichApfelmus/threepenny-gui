{-# LANGUAGE CPP, PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Exception
import Control.Monad
import Data.List.Extra
import Data.Time
#ifdef CABAL
import "threepenny-gui" Graphics.UI.Threepenny as UI
#else
import Graphics.UI.Threepenny as UI
#endif
import Prelude hiding (catch)


main :: IO ()
main = do
    messages <- Chan.newChan
    startGUI Config
        { tpPort       = 10000
        , tpCustomHTML = Just "chat.html"
        , tpStatic     = "../wwwroot"
        } $ setup messages

type Message = (UTCTime, String, String)

setup :: Chan Message -> Window -> IO () 
setup globalMsgs w = do
    return w # set title "Chat"

    body <- getBody w
    
    new w # set cssClass "header"   # set text "Threepenny Chat" # appendTo body
    new w # set cssClass "gradient" # appendTo body
    codeLink w body
    
    nickname    <- mkNickname w body
    messageArea <- new w # set cssClass "message-area" # appendTo body
    msgs        <- Chan.dupChan globalMsgs
    sendMessageArea w body nickname msgs
    void $ forkIO $ receiveMessages w msgs messageArea
    

--    io $ catch (runTP session handleEvents)
--             (\e -> do killThread messageReceiver
--                       throw (e :: SomeException))

receiveMessages w msgs messageArea = do
    messages <- Chan.getChanContents msgs
    forM_ messages $ \msg -> do
        atomic w $ do
          newMessage w msg # appendTo messageArea
          scrollToBottom messageArea

sendMessageArea w parent nickname msgs = do
    sendArea <- new w      # set cssClass "send-area"     # appendTo parent
    input    <- textarea w # set cssClass "send-textarea" # appendTo sendArea
    onSendValue input $ \(trim -> content) -> do
        when (not (null content)) $ do
            now <- getCurrentTime
            (trim -> nick) <- get value nickname
            element input # set value ""
            when (not (null nick)) $
                Chan.writeChan msgs (now,nick,content)

mkNickname w parent = do
    myname <- new w # set cssClass "name-area" # appendTo parent
    UI.span w # set cssClass "name-label"
              # set text "Your name "
              # appendTo myname
    input <- UI.input w # set cssClass "name-input" # appendTo myname
    setFocus input

newMessage :: Window -> Message -> IO Element
newMessage w (timestamp,nick,content) = do
  msg <- new w # set cssClass "message"
  new w # set cssClass "timestamp" # set text (show timestamp)   # appendTo msg
  new w # set cssClass "name"      # set text (nick ++ " says:") # appendTo msg
  new w # set cssClass "content"   # set text content            # appendTo msg
  return msg

codeLink w parent = void $ do
    let url = "https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/src/Chat.hs"
    anchor w
        # set (attr "href") url
        # set text "View source code"
        # set cssClass "code-link"
        # appendTo parent
