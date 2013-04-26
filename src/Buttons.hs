{-# LANGUAGE CPP, PackageImports #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

import Control.Monad
import Control.Concurrent (threadDelay)

#ifdef CABAL
import "threepenny-gui" Graphics.UI.Threepenny
#else
import Graphics.UI.Threepenny
#endif

-- | Main entry point. Starts a TP server.
main :: IO ()
main = startGUI Config
    { tpPort       = 10000
    , tpCustomHTML = Nothing
    , tpStatic     = "../wwwroot"
    } setup

-- | A per-user worker thread. Each user session has a thread.
setup :: Window -> IO ()
setup w = do
    return w # set title "Buttons"
    
    body <- getBody w
    addStyleSheet w "buttons.css"
    wrap <- new w
                # set cssClass "wrap"
                # appendTo body
    
    greet       w wrap
    makeButtons w wrap
    codeLink    w wrap

greet :: Window -> Element -> IO ()
greet w body = void $ do
    h1 w
        # set text "Hello, Haskell!"
        # appendTo body
    new w
        # set text "Try the buttons below, they hover and click."
        # appendTo body

codeLink :: Window -> Element -> IO ()
codeLink w body = void $ do
    p <- paragraph w
        # set text ""
        # appendTo body
         
    anchor w
        # set (attr "href") "https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/src/Buttons.hs"
        # set text "View source code"
        # set cssClass "view-source"
        # appendTo p

makeButtons :: Window -> Element -> IO ()
makeButtons w body = void $ do
    list <- ul w
        # set cssClass "buttons-list"
        # appendTo body
    
    button1 <- appendToButton w body button1Title
    
    on hover button1 $ \_ -> void $ do
        element button1 # set text (button1Title ++ " [hover]")
    on leave button1 $ \_ -> void $ do
        element button1 # set text button1Title
    on click button1 $ \_ -> void $ do
        threadDelay $ 1000 * 1000 * 1
        element button1
            # set text (button1Title ++ " [pressed]")
        li w
            # set html "<b>Delayed</b> result!"
            # appendTo list
    
    button2 <- appendToButton w body button2Title

    on hover button2 $ \_ -> do
        element button2 # set text (button2Title ++ " [hover]")
    on leave button2 $ \_ -> do
        element button2 # set text button2Title
    on click button2 $ \_ -> do
        element button1
            # set text (button1Title ++ " [pressed]")
        li w
            # set html "Zap! Quick result!"
            # appendTo list

  where button1Title = "Click me, I delay a bit"
        button2Title = "Click me, I work immediately"

appendToButton :: Window -> Element -> String -> IO Element
appendToButton w body caption = do
    p <- paragraph w
        # appendTo body
    button w
        # set text caption
        # set cssClass "button"
        # appendTo p

