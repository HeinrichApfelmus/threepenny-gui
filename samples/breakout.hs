import Data.Time
import Control.Monad
-- import Prelude hiding (catch,div,span)

import Paths

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Attributes as A
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
        # set A.id_ "myCanvas"
        # set style [("border", "solid black 1px")]

    let horFill = C.createHorizontalLinearGradient (C.RGB 255 0 0) (C.RGB 0 255 0)
    let diagFill = C.createLinearGradient 1 1 [(0, C.RGB 255 0 0), (0.5, C.RGB 0 255 0), (1, C.RGB 0 0 255)]
    let solidFill = C.solidColor $ C.RGB 0 0 255

    C.fillRect (C.Rect 30 40 100 10) solidFill canvas

    C.fillRect (C.Rect 100 110 100 20) horFill canvas
    C.fillRect (C.Rect 100 150 100 100) diagFill canvas

    return canvas    
