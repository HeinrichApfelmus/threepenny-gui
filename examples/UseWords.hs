{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Main where

import Control.Concurrent
import Control.Monad.Extra
import Control.Monad.IO
import Graphics.UI.Ji
import Graphics.UI.Ji.DOM

main :: IO ()
main = serve Config
    { jiPort = 10003
    , jiRun = runJi
    , jiWorker = worker
    , jiInitHTML = "use-words.html"
    , jiStatic = "wwwroot"
    }

worker :: MonadJi m => m ()
worker = do
  handleEvents
