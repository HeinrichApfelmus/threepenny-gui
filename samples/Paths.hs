{-# LANGUAGE CPP#-}
module Paths (getStaticDir, samplesURL) where

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

-- | Base URL for the example source code.
samplesURL :: String
samplesURL = "https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/samples/"
