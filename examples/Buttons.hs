{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Main where

import Control.Concurrent
import Control.Monad.IO
import Graphics.UI.Ji
import Graphics.UI.Ji.Types

-- | Main entry point. Starts a ji server.
main :: IO ()
main = serve 10001 runJi worker

-- | A per-user worker thread. Each user session has a thread.
worker :: MonadJi m => m ()
worker = do
  els <- getElementByTagName "body"
  case els of
    Nothing -> error "Where's the body?"
    Just body -> do
      debug $ "Got body: " ++ show body
      setStyle body [("background","#333")]
      greet body
      makeButtons body
  handleEvents

greet :: MonadJi m => Element -> m ()
greet body = do
  setStyle body [("color","#ccc")]
  
  header <- newElement "h2"
  append body header
  setText header "Hello, Haskell!"
  
  greeting <- newElement "div"
  setText greeting "Try the buttons below, they hover and click."
  append body greeting

makeButtons :: MonadJi m => Element -> m ()
makeButtons body = do
  list <- newElement "ul"
  setStyle list [("color","#aaa")]
  
  button1 <- appendButton body button1Title
  button2 <- appendButton body button2Title
  append body list
    
  onHover button1 $ \_ -> do setText button1 (button1Title ++ " [hover]")
  onBlur button1  $ \_ -> do setText button1 button1Title
  onClick button1 $ \_ -> do
    li <- newElement "li"
    io $ threadDelay $ 1000 * 1000 * 1
    setText button1 (button1Title ++ " [pressed]")
    setHtml li "<b>Delayed</b> result!"
    append list li

  onHover button2 $ \_ -> do setText button2 (button2Title ++ " [hover]")
  onBlur button2  $ \_ -> do setText button2 button2Title
  onClick button2 $ \_ -> do
    li <- newElement "li"
    setText button2 (button2Title ++ " [pressed]")
    setHtml li "Zap! Quick result!"
    append list li

  where button1Title = "Click me, I delay a bit"
        button2Title = "Click me, I work immediately"

appendButton :: MonadJi m => Element -> String -> m Element
appendButton body caption = do
  p <- newElement "p"
  button <- newElement "a"
  append body p
  append p button
  setText button caption
  setStyle button [("cursor","pointer")
                  ,("color","#acc2a1")
                  ,("text-decoration","underline")]
  return button
