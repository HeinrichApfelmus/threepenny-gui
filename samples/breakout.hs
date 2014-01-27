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

    (eFC, bFC) <- frameClock (Hz 60) 
    onEvent eFC $ showFPS localTime 

    let movePaddle  = (\x -> \(_, ball) -> (x, ball)) <$> mouseX world
        moveBall    = (\dt -> \(x, ball) -> (x, animateBall world dt ball)) <$> eFC
        changeSzene = unionWith (.) movePaddle moveBall
        startBall   = Ball (Pos 40 40) (Vel 400 150)
    bSzene <- accumB (0, startBall) changeSzene

    onChanges bSzene (updateCanvas world)


{-----------------------------------------------------------------------------
    Animation engine
------------------------------------------------------------------------------}

data Interval  = Ms Int
data FrameRate = Hz Int

type PosX      = Double
type PosY      = Double
data Position  = Pos { posX :: PosX, posY :: PosY }
data Velocity  = Vel { velX :: Double, velY :: Double }

data Moving    = Mov { movPos :: Position, movV :: Velocity }

animate :: MonadIO m => state -> (state -> Interval -> state) -> Event Interval -> m (Behavior state)
animate start anim frames = 
  accumB start (flip anim <$> frames)

animateMoving :: MonadIO m => Position -> Velocity -> (Velocity -> Position -> Velocity) -> Event Interval -> m (Behavior Moving)
animateMoving startPos startV changeV frames =
  animate (Mov startPos startV) animMove frames
  where animMove (Mov (Pos px py) v@(Vel vx vy)) i@(Ms dt) = Mov p' v'
          where p'  = Pos (px + dt' * vx) (py + dt' * vy)
                v'  = changeV v p'
                dt' = fromIntegral dt

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

type PaddlePos = PosX
data Ball      = Ball { ballPos   :: Position
                      , ballVel   :: Velocity
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

animateBall :: World -> Interval -> Ball -> Ball
animateBall w (Ms ms) (Ball (Pos px py) v@(Vel vx vy)) = Ball p'' v'
  where p'        = Pos (px + dt * vx) (py + dt * vy)
        (p'', v') = bounce p'
        dt        = if ms /= 0 then fromIntegral ms / 1000 else 0
        bounce (Pos x y) = (Pos px' py', Vel vx' vy')
            where (px', vx') = if x < 0 then (0, negate vx) else if x > width then (width, negate vx) else (x , vx)
                  (py', vy') = if y < 0 then (0, negate vy) else if y > height then (height, negate vy) else (y , vy)
        (width, height) = ((fromIntegral . szWidth . screenSize $ w), (fromIntegral . szHeight . screenSize $ w))

updateCanvas :: World -> (PaddlePos, Ball) -> UI ()
updateCanvas world (p, ball) = do
    let c  = drawCanvas world
        sz = screenSize world
        setFill fs = 
           return c # set C.fillStyle fs

    setFill white
    C.fillRect (0, 0) (fromIntegral . szWidth $ sz) (fromIntegral . szHeight $ sz) c

    setFill fill
    C.fillRect (p-w2, y0) w h c

    setFill red
    C.fillRect (posX (ballPos ball) - 4, posY (ballPos ball) - 4) 8 8 c

    toScreen world

    where fill   = C.createHorizontalLinearGradient (p-w2, y0) w  (C.RGB 255 10 10) (C.RGB 10 10 255)
          white  = C.solidColor (C.RGB 255 255 255)
          red    = C.solidColor (C.RGB 255 0 0)
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
