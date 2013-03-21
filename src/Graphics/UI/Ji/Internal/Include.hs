module Graphics.UI.Ji.Internal.Include (include) where
 
import Data.Functor
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

include = QuasiQuoter { quoteExp = f }
    where
    f s = TH.LitE . TH.StringL <$> TH.runIO (readFile s)
