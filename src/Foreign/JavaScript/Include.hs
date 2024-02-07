{-# LANGUAGE TemplateHaskell, CPP #-}
module Foreign.JavaScript.Include (include) where

import           Data.FileEmbed      (makeRelativeToProject)
import           Language.Haskell.TH
import           System.IO

include :: FilePath -> Q Exp
include path = do
    relativePath <- makeRelativeToProject path
    LitE . StringL <$> runIO (readFileUTF8 relativePath)

readFileUTF8 :: FilePath -> IO String
readFileUTF8 path = do
    h <- openFile path ReadMode
    hSetEncoding h utf8
    hGetContents h
