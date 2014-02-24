{-# LANGUAGE TemplateHaskell, CPP #-}
module Graphics.UI.Threepenny.Internal.Include (include) where
 
import Data.Functor
import System.IO
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

#if defined(CABAL) || defined(FPCOMPLETE)
root :: FilePath
root = "src/"
#else
root :: FilePath
root = "../src/" -- running examples from ghci
#endif

include :: QuasiQuoter
include = QuasiQuoter
        { quoteExp  = f             -- only used as an expression,
        , quotePat  = undefined     -- hence all other use cases undefined
        , quoteType = undefined
        , quoteDec  = undefined
        }
    where
    f s = TH.LitE . TH.StringL <$> TH.runIO (readFileUTF8 $ root ++ s)

readFileUTF8 :: FilePath -> IO String
readFileUTF8 path = do
    h <- openFile path ReadMode
    hSetEncoding h utf8
    hGetContents h
