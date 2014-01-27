import Data.Time
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone)

import Control.Monad
-- import Prelude hiding (catch,div,span)

import Paths

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Events as E
import qualified Graphics.UI.Threepenny.Attributes as A
import qualified Graphics.UI.Threepenny.Canvas as C
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Timer as T

{-----------------------------------------------------------------------------
    simple breakout clone using canvas
------------------------------------------------------------------------------}

main :: IO ()
main = do
    static   <- getStaticDir
    startGUI defaultConfig
        { tpPort = Just 10000
        , tpStatic = Just static
        } setup

setup :: Window -> UI ()
setup w = void $ do
    return w # set title "threepenny-breaktout..."

    world <- mkWorld (Size 800 600)
    localTime <- string "---"
    let intoDiv els = UI.div #+ map element els
    getBody w #+ [intoDiv [localTime], intoDiv [screenCanvas world]]

    onEvent (mouseX world) (updateCanvas world)

    (eFC, bFC) <- frameClock (Hz 60) 
    onEvent eFC $ showFPS localTime 

data Interval  = Ms Int
data FrameRate = Hz Int

fromFrameRate :: FrameRate -> Interval
fromFrameRate (Hz fr) = Ms $ 1000 `div` fr

fromInterval :: Interval -> FrameRate
fromInterval (Ms ms)
  | ms > 10   = Hz $ 1000 `div` ms
  | otherwise = Hz 1

frameClock :: FrameRate -> UI (Event Interval, Behavior UTCTime)
frameClock fr = do
    startTime <- liftIO getCurrentTime
    let (Ms ms) = fromFrameRate fr
        mapFrameTime :: () -> IO (UTCTime -> (Interval, UTCTime))
        mapFrameTime () = do
          cT <- getCurrentTime
          return $ \lT -> (Ms . ceiling $ (diffUTCTime cT lT) * 1000, cT)
    t <- T.timer
    return t # set T.interval ms
    T.start t
    mapAccum startTime (unsafeMapIO mapFrameTime $ T.tick t)
    
showTime :: Element -> UTCTime -> UI ()
showTime intoEl time = do
    tZone       <- liftIO getCurrentTimeZone 
    let timeVal = time
        locTime = utcToLocalTime tZone timeVal
        tod     = localTimeOfDay locTime
        (h,m,s) = (todHour tod, todMin tod, todSec tod)
        fT      = show h ++ ":" ++ show m ++ ":" ++ show (floor s)
    element intoEl # set UI.text fT
    return ()

showFPS :: Element -> Interval -> UI ()
showFPS intoEl interv = do
    let (Hz fps) = fromInterval interv
        fFps     = "FPS: " ++ show fps
    element intoEl # set UI.text fFps
    return ()

{-----------------------------------------------------------------------------
    Model
------------------------------------------------------------------------------}
type PosX      = Double
type PosY      = Double
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
                   , mouseX       :: Event Double
                   , drawCanvas   :: C.Canvas
                   , screenCanvas :: C.Canvas
                   , toScreen     :: UI ()
                   }

mkWorld :: Size -> UI World
mkWorld screenSize = do
    dc <- mkCanvas screenSize
    sc <- mkCanvas screenSize

    -- paddlePos <- stepper (szWidth sz `div` 2) $ (fst <$> Ev.mousemove canvas)
    let mouseX = fromIntegral . fst <$> E.mousemove sc
    let sw = C.drawImage dc (0,0) sc
    return $ World screenSize mouseX dc sc sw

updateCanvas :: World -> PaddlePos -> UI ()
updateCanvas world p = do
    let c  = drawCanvas world
        sz = screenSize world
        setFill fs = 
           return c # set C.fillStyle fs

    setFill white
    C.fillRect (0, 0) (fromIntegral . szWidth $ sz) (fromIntegral . szHeight $ sz) c

    setFill fill
    C.fillRect (p-w2, y0) w h c
    toScreen world

    where fill   = C.createHorizontalLinearGradient (p-w2, y0) w  (C.RGB 255 10 10) (C.RGB 10 10 255)
          white  = C.solidColor (C.RGB 255 255 255)
          y0     = fromIntegral (szHeight . screenSize $ world) - 2*h
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
