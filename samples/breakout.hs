import Data.Time
import Control.Monad
-- import Prelude hiding (catch,div,span)

import Paths

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Canvas as C
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    simple breakout clone using canvas
------------------------------------------------------------------------------}

main :: IO ()
main = do
    static   <- getStaticDir
    startGUI defaultConfig
        { tpPort = 10000
        , tpStatic = Just static
        } setup

setup :: Window -> UI ()
setup w = void $ do
    return w # set title "threepenny-breaktout..."

    c     <- mkCanvas
    getBody w #+ [element c]

{-----------------------------------------------------------------------------
    Model
------------------------------------------------------------------------------}
{-----------------------------------------------------------------------------
    View
------------------------------------------------------------------------------}

-- | Create a canvas
mkCanvas :: UI Element
mkCanvas = do
    canvas <- UI.canvas
        # set UI.height 640
        # set UI.width  800
        # set style [("border", "solid black 1px")]

    return canvas    
