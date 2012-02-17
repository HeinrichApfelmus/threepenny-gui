{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Main where

import Control.Concurrent
import Control.Monad.Extra
import Control.Monad.IO
import Graphics.UI.Ji

-- | Main entry point. Starts a ji server.
main :: IO ()
main = serve Config
    { jiPort = 10001
    , jiRun = runJi
    , jiWorker = worker
    , jiStatic = "wwwroot"
    }

-- | A per-user worker thread. Each user session has a thread.
worker :: MonadJi m => m ()
worker = do
  setTitle "Buttons"
  body <- getElementByTagName "body"
  whenJust body $ \body -> do
    setStyle [("background","#333")] body
    greet body
    makeButtons body
  handleEvents

greet :: MonadJi m => Element -> m ()
greet body = do
  setStyle [("color","#ccc")] body
  
  header <- newElement "h2"
  appendTo body header
  setText "Hello, Haskell!" header
  
  p <- newElementText body "p" ""
  vex <- link "https://github.com/chrisdone/ji/blob/master/examples/Buttons.hs"
              "Buttons.hs"
  appendTo p vex

  greeting <- newElement "div"
  setText "Try the buttons below, they hover and click." greeting
  appendTo body greeting
  return ()

makeButtons :: MonadJi m => Element -> m ()
makeButtons body = do
  list <- newElement "ul"
  setStyle [("color","#aaa")] list
  
  button1 <- appendToButton body button1Title
  button2 <- appendToButton body button2Title
  appendTo body list
    
  onHover button1 $ \_ -> do setText (button1Title ++ " [hover]") button1; return ()
  onBlur button1  $ \_ -> do setText button1Title button1; return ()
  onClick button1 $ \_ -> do
    li <- newElement "li"
    io $ threadDelay $ 1000 * 1000 * 1
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

appendToButton :: MonadJi m => Element -> String -> m Element
appendToButton body caption = do
  p <- newElement "p"
  button <- newElement "a"
  appendTo body p
  appendTo p button
  setText caption button
  setStyle [("cursor","pointer")
           ,("color","#acc2a1")
           ,("text-decoration","underline")]
           button
  return button

link :: MonadJi m => String -> String -> m Element
link url text = do
  el <- newElement "a"
  setAttr "href" url el
  setText text el
  setStyle [("color","#acc2a1")] el
  return el

newElementText :: MonadJi m => Element -> String -> String -> m Element
newElementText parent tagName text = do
  el <- newElement tagName
  appendTo parent el
  setText text el
  return el
