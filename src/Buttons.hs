{-# LANGUAGE RecordWildCards, CPP, PackageImports #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Main where

import Control.Concurrent
import Control.Monad.Extra
import Control.Monad.IO.Class
#ifdef CABAL
import "threepenny-gui" Graphics.UI.Threepenny
import "threepenny-gui" Graphics.UI.Threepenny.Browser
#else
import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.Browser
#endif

-- | Main entry point. Starts a TP server.
main :: IO ()
main = serve Config
    { tpPort = 10001
    , tpWorker = \window -> setup window >> handleEvents window
    , tpInitHTML = Nothing
    , tpStatic = "wwwroot"
    }

-- | A per-user worker thread. Each user session has a thread.
setup :: Window -> IO ()
setup w = do
  setTitle w "Buttons"
  body <- getBody w
  wrap <- newElement w "div" >>= setAttr "class" "wrap" >>= appendTo body
  greet w wrap
  makeButtons w wrap
  linkage w wrap

greet :: Window -> Element -> IO ()
greet w body = do
  header <- newElement w "h1"
  appendTo body header
  setText "Hello, Haskell!" header
  
  greeting <- newElement w "div"
  setText "Try the buttons below, they hover and click." greeting
  appendTo body greeting
  return ()

linkage :: Window -> Element -> IO ()
linkage w body = do
  p <- newElementText w body "p" ""
  vex <- link w "https://github.com/chrisdone/ji/blob/master/examples/Buttons.hs"
              "View source code" >>= setAttr "class" "view-source"
  appendTo p vex
  return ()

makeButtons :: Window -> Element -> IO ()
makeButtons w body = do
  list <- newElement w "ul"
  setAttr "class" "buttons-list" list
  
  button1 <- appendToButton w body button1Title
  button2 <- appendToButton w body button2Title
  appendTo body list
    
  onHover button1 $ \_ -> do setText (button1Title ++ " [hover]") button1; return ()
  onBlur button1  $ \_ -> do setText button1Title button1; return ()
  onClick button1 $ \_ -> do
    li <- newElement w "li"
    threadDelay $ 1000 * 1000 * 1
    setText (button1Title ++ " [pressed]")  button1
    setHtml "<b>Delayed</b> result!" li
    appendTo list li
    return ()

  onHover button2 $ \_ -> do setText (button2Title ++ " [hover]") button2; return ()
  onBlur button2  $ \_ -> do setText button2Title button2; return ()
  onClick button2 $ \_ -> do
    li <- newElement w "li"
    setText (button2Title ++ " [pressed]") button2
    setHtml "Zap! Quick result!" li
    appendTo list li
    return ()

  where button1Title = "Click me, I delay a bit"
        button2Title = "Click me, I work immediately"

appendToButton :: Window -> Element -> String -> IO Element
appendToButton w body caption = do
  p <- newElement w "p"
  button <- newElement w "a"
  appendTo body p
  appendTo p button
  setText caption button
  setAttr "class" "button" button
  return button

link :: Window -> String -> String -> IO Element
link w url text = do
  el <- newElement w "a"
  setAttr "href" url el
  setText text el
  return el

newElementText :: Window -> Element -> String -> String -> IO Element
newElementText w parent tagName text = do
  el <- newElement w tagName
  appendTo parent el
  setText text el
  return el
