{-# LANGUAGE CPP#-}
module Paths (getStaticDir) where

import Control.Monad
import System.FilePath

#if defined(CABAL)
-- using cabal
import qualified Paths_threepenny_gui (getDataDir)

getStaticDir :: IO FilePath
getStaticDir = (</> "wwwroot") `liftM` Paths_threepenny_gui.getDataDir

#elif defined(FPCOMPLETE)

getStaticDir :: IO FilePath
getStaticDir = return "wwwroot"

#else
-- using GHCi

getStaticDir :: IO FilePath
getStaticDir = return "../wwwroot/"

#endif
