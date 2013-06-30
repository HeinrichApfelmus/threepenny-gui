{-# LANGUAGE CPP, PackageImports #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Exception
import Control.Monad
import Data.Functor
import Data.List.Extra
import Data.Time
import Prelude hiding (catch)

import Control.Monad.Trans.Reader as Reader
import Control.Monad.IO.Class

#ifdef CABAL
import qualified "threepenny-gui" Graphics.UI.Threepenny as UI
import "threepenny-gui" Graphics.UI.Threepenny.Core hiding (text)
#else
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (text)
#endif
import Paths

{-----------------------------------------------------------------------------
    Chat
------------------------------------------------------------------------------}

main :: IO ()
main = do
    static   <- getStaticDir
    messages <- Chan.newChan
    startGUI Config
        { tpPort       = 10000
        , tpCustomHTML = Just "chat.html"
        , tpStatic     = static
        } $ setup messages

type Message = (UTCTime, String, String)

setup :: Chan Message -> Window -> IO ()
setup globalMsgs w = do
    msgs <- Chan.dupChan globalMsgs

    return w # set title "Chat"
    
    (nick, nickname) <- mkNickname
    messageArea      <- mkMessageArea msgs nick

    getBody w #+
        [ UI.div #. "header"   #+ [string "Threepenny Chat"]
        , UI.div #. "gradient"
        , viewSource
        , element nickname
        , element messageArea
        ]
    
    void $ forkIO $ receiveMessages w msgs messageArea

--    io $ catch (runTP session handleEvents)
--             (\e -> do killThread messageReceiver
--                       throw (e :: SomeException))

receiveMessages w msgs messageArea = do
    messages <- Chan.getChanContents msgs
    forM_ messages $ \msg -> do
        atomic w $ do
          element messageArea #+ [mkMessage msg]
          UI.scrollToBottom messageArea

mkMessageArea :: Chan Message -> Element -> IO Element
mkMessageArea msgs nickname = do
    input <- UI.textarea #. "send-textarea"
    
    on UI.sendValue input $ (. trim) $ \content -> do
        when (not (null content)) $ do
            now  <- getCurrentTime
            nick <- trim <$> get value nickname
            element input # set value ""
            when (not (null nick)) $
                Chan.writeChan msgs (now,nick,content)

    UI.div #. "message-area" #+ [UI.div #. "send-area" #+ [element input]]


mkNickname :: IO (Element, Element)
mkNickname = do
    i  <- UI.input #. "name-input"
    el <- UI.div   #. "name-area"  #+
        [ UI.span  #. "name-label" #+ [string "Your name "]
        , element i
        ]
    UI.setFocus i
    return (i,el)

mkMessage :: Message -> IO Element
mkMessage (timestamp, nick, content) =
    UI.div #. "message" #+
        [ UI.div #. "timestamp" #+ [string $ show timestamp]
        , UI.div #. "name"      #+ [string $ nick ++ " says:"]
        , UI.div #. "content"   #+ [string content]
        ]

viewSource :: IO Element
viewSource =
    UI.anchor #. "view-source" # set UI.href url #+ [string "View source code"]
    where
    url = "https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/src/Chat.hs"
