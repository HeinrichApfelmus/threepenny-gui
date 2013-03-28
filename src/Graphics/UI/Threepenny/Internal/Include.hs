{-# LANGUAGE CPP #-}
module Graphics.UI.Threepenny.Internal.Include (include) where
 
import Data.Functor
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

#ifdef CABAL
root = "src/"
#else
root = ""
#endif

include = QuasiQuoter { quoteExp = f }
    where
    f s = TH.LitE . TH.StringL <$> TH.runIO (readFile $ root ++ s)
