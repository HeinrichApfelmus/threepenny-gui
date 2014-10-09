{-# LANGUAGE TemplateHaskell, CPP #-}
module Foreign.JavaScript.Include (include) where
 
import           Data.Functor
import qualified Language.Haskell.TH       as TH
import           Language.Haskell.TH.Quote
import           System.IO


#if defined(SAMPLES)
root :: FilePath
root = "../" -- we are running the examples from ghci
#else
root :: FilePath
root = "./"
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
