{-# OPTIONS -Wall #-}

module Control.Monad.IO where

import Control.Monad.IO.Class

io :: MonadIO m => IO a -> m a
io = liftIO
