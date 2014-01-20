import Data.Time
import Control.Monad
-- import Prelude hiding (catch,div,span)

import Paths

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Events as E
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

    world <- mkWorld (Size 800 600)
    getBody w #+ [element (screenCanvas world)]

    onEvent (mouseX world) (updateCanvas world)

{-----------------------------------------------------------------------------
    Model
------------------------------------------------------------------------------}
type PosX      = Int
type PosY      = Int
type DPosX     = Double
type DPosY     = Double
type Speed     = Double

type PaddlePos = PosX
data Ball      = Ball { ballPos   :: (PosX, PosY)
                      , ballVel   :: (DPosX, DPosY)
                      , ballSpeed :: Speed 
                      }

data Size  = Size  { szWidth :: Int
                   , szHeight :: Int }

data World = World { screenSize   :: Size
                   , mouseX       :: Event Int
                   , drawCanvas   :: C.Canvas
                   , screenCanvas :: C.Canvas
                   , toScreen     :: UI ()
                   }

mkWorld :: Size -> UI World
mkWorld screenSize = do
    dc <- mkCanvas screenSize
    sc <- mkCanvas screenSize

    -- paddlePos <- stepper (szWidth sz `div` 2) $ (fst <$> Ev.mousemove canvas)
    let mouseX = fst <$> E.mousemove sc
    let sw = C.drawImage dc (0,0) sc
    return $ World screenSize mouseX dc sc sw

updateCanvas :: World -> PaddlePos -> UI ()
updateCanvas world p = do
    let c  = drawCanvas world
        sz = screenSize world
    C.fillRect (C.Rect 0 0 (szWidth sz) (szHeight sz)) white c
    C.fillRect paddle fill c
    toScreen world
    where paddle = C.Rect (p-w2) y0 w h
          fill   = C.solidColor (C.RGB 255 10 10)
          white  = C.solidColor (C.RGB 255 255 255)
          y0     = szHeight (screenSize world) - 2*h
          w      = 2 * w2
          w2     = 25
          h      = 10


{-----------------------------------------------------------------------------
    View
------------------------------------------------------------------------------}

-- | Create a canvas
mkCanvas :: Size -> UI C.Canvas
mkCanvas sz = do
    canvas <- UI.canvas
        # set UI.height (szHeight sz)
        # set UI.width  (szWidth sz)
        # set A.id_ "myCanvas"
        # set style [("border", "solid black 1px")]

    return canvas    
