-- | Extra utilities for chans.

module Control.Concurrent.Chan.Extra
  (readAvailableChan)
  where

import Control.Concurrent.Chan

readAvailableChan :: Chan a -> IO [a]
readAvailableChan chan = do
  v <- readChan chan
  vs <- rest
  return (v:vs)

  where
    rest = do
      empty <- isEmptyChan chan
      if empty
         then return []
         else do v <- readChan chan
                 vs <- rest
                 return (v:vs)
