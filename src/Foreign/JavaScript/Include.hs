{-# LANGUAGE TemplateHaskell, CPP #-}
module Foreign.JavaScript.Include (include) where

import           Data.Functor
import           Data.FileEmbed      (makeRelativeToProject)
import           Language.Haskell.TH
import           System.IO

include :: FilePath -> Q Exp
include path = do
    path <- makeRelativeToProject path
    LitE . StringL <$> runIO (readFileUTF8 path)

readFileUTF8 :: FilePath -> IO String
readFileUTF8 path = do
    h <- openFile path ReadMode
    hSetEncoding h utf8
    hGetContents h
