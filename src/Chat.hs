{-# LANGUAGE CPP, PackageImports #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Exception
import Control.Monad
import Data.Functor
import Data.List.Extra
import Data.Time
import Prelude hiding (catch,div,span)

import Control.Monad.Trans.Reader as Reader
import Control.Monad.IO.Class

#ifdef CABAL
import "threepenny-gui" Graphics.UI.Threepenny as UI
#else
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (text)
-- import qualified Graphics.UI.Threepenny.Internal.Types as UI
import Graphics.UI.Threepenny.Elements (span, div, textarea, input, anchor)
import Graphics.UI.Threepenny.JQuery
import Graphics.UI.Threepenny.Properties
#endif
import Paths


{-----------------------------------------------------------------------------
    HTML utilities
------------------------------------------------------------------------------}
-- Make a @span@ element with a given text content.
text :: String -> Dom Element
text s = ReaderT $ \w -> newElement w "span" # set UI.text s

{-----------------------------------------------------------------------------
    Main application
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
    
    (nick, nickname) <- withWindow w $ mkNickname
    messageArea      <- withWindow w $ mkMessageArea msgs nick

    body <- getBody w
    element body #+
        [ div #. "header"   #+ [text "Threepenny Chat"]
        , div #. "gradient"
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
          scrollToBottom messageArea

mkMessageArea :: Chan Message -> Element -> Dom Element
mkMessageArea msgs nickname = do
    input <- textarea #. "send-textarea"
    
    liftIO $ onSendValue input $ (. trim) $ \content -> do
        when (not (null content)) $ do
            now  <- getCurrentTime
            nick <- trim <$> get value nickname
            element input # set value ""
            when (not (null nick)) $
                Chan.writeChan msgs (now,nick,content)

    div #. "message-area" #+ [div #. "send-area" #+ [element input]]


mkNickname :: Dom (Element, Element)
mkNickname = do
    i  <- input #. "name-input"
    el <- div   #. "name-area"  #+
        [ span  #. "name-label" #+ [text "Your name "]
        , element i
        ]
    liftIO $ setFocus i
    return (i,el)

mkMessage :: Message -> Dom Element
mkMessage (timestamp, nick, content) =
    div #. "message" #+
        [ div #. "timestamp" #+ [text $ show timestamp]
        , div #. "name"      #+ [text $ nick ++ "says:"]
        , div #. "content"   #+ [text content]
        ]

viewSource :: Dom Element
viewSource = anchor #. "view-source" # set UI.href url #+ [text "View source code"]
    where
    url = "https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/src/Chat.hs"
