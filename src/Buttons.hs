{-# LANGUAGE RecordWildCards, CPP, PackageImports #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Main where

import Control.Concurrent
import Control.Monad.Extra
import Control.Monad.IO.Class
#ifdef CABAL
import "threepenny-gui" Graphics.UI.Threepenny
#else
import Graphics.UI.Threepenny
#endif

-- | Main entry point. Starts a TP server.
main :: IO ()
main = serve Config
    { tpPort = 10001
    , tpRun = runTP
    , tpWorker = worker
    , tpInitHTML = Just "buttons.html"
    , tpStatic = "wwwroot"
    }

-- | A per-user worker thread. Each user session has a thread.
worker :: MonadTP m => m ()
worker = do
  setTitle "Buttons"
  body <- getBody
  wrap <- newElement "div" >>= setAttr "class" "wrap" >>= appendTo body
  greet wrap
  makeButtons wrap
  linkage wrap
  handleEvents

greet :: MonadTP m => Element -> m ()
greet body = do
  header <- newElement "h1"
  appendTo body header
  setText "Hello, Haskell!" header
  
  greeting <- newElement "div"
  setText "Try the buttons below, they hover and click." greeting
  appendTo body greeting
  return ()

linkage :: MonadTP m => Element -> m ()
linkage body = do
  p <- newElementText body "p" ""
  vex <- link "https://github.com/chrisdone/ji/blob/master/examples/Buttons.hs"
              "View source code" >>= setAttr "class" "view-source"
  appendTo p vex
  return ()

makeButtons :: MonadTP m => Element -> m ()
makeButtons body = do
  list <- newElement "ul"
  setAttr "class" "buttons-list" list
  
  button1 <- appendToButton body button1Title
  button2 <- appendToButton body button2Title
  appendTo body list
    
  onHover button1 $ \_ -> do setText (button1Title ++ " [hover]") button1; return ()
  onBlur button1  $ \_ -> do setText button1Title button1; return ()
  onClick button1 $ \_ -> do
    li <- newElement "li"
    liftIO $ threadDelay $ 1000 * 1000 * 1
    setText (button1Title ++ " [pressed]")  button1
    setHtml "<b>Delayed</b> result!" li
    appendTo list li
    return ()

  onHover button2 $ \_ -> do setText (button2Title ++ " [hover]") button2; return ()
  onBlur button2  $ \_ -> do setText button2Title button2; return ()
  onClick button2 $ \_ -> do
    li <- newElement "li"
    setText (button2Title ++ " [pressed]") button2
    setHtml "Zap! Quick result!" li
    appendTo list li
    return ()

  where button1Title = "Click me, I delay a bit"
        button2Title = "Click me, I work immediately"

appendToButton :: MonadTP m => Element -> String -> m Element
appendToButton body caption = do
  p <- newElement "p"
  button <- newElement "a"
  appendTo body p
  appendTo p button
  setText caption button
  setAttr "class" "button" button
  return button

link :: MonadTP m => String -> String -> m Element
link url text = do
  el <- newElement "a"
  setAttr "href" url el
  setText text el
  return el

newElementText :: MonadTP m => Element -> String -> String -> m Element
newElementText parent tagName text = do
  el <- newElement tagName
  appendTo parent el
  setText text el
  return el
