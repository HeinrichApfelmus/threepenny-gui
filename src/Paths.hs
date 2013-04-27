{-# LANGUAGE CPP#-}
module Paths (getStaticDir) where

import Control.Monad
import System.FilePath

#if CABAL
-- using cabal
import qualified Paths_threepenny_gui (getDataDir)

getStaticDir :: IO FilePath
getStaticDir = (</> "wwwroot") `liftM` Paths_threepenny_gui.getDataDir

#else
-- using GHCi

getStaticDir :: IO FilePath
getStaticDir = return "../wwwroot/"

#endif
