import Data.Time
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import Data.Maybe (catMaybes)
import Data.List ((\\))

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

    (eFC, bFC) <- frameClock (Hz 120) 
    onEvent eFC $ showFPS localTime 

    let updatePaddlePos _ gs@(GS _ _ _ Lost)         = gs
        updatePaddlePos x (GS _ ball bricks Running) = (GS x ball bricks Running)
        updateBallPos _ gs@(GS _ _ _ Lost)           = gs
        updateBallPos t gs@(GS _ _ _ Running)        = animateBall world t gs

    let movePaddle  = updatePaddlePos <$> mouseX world
        moveBall    = updateBallPos <$> eFC
        changeSzene = unionWith (.) movePaddle moveBall
        start       = GS 0 (Ball (Pos 40 300) (Vel 300 120)) (initBricks world) Running

    bSzene <- accumB start changeSzene

    onChanges bSzene (updateCanvas world)


{-----------------------------------------------------------------------------
    Animation engine
------------------------------------------------------------------------------}

data Interval  = Ms Int
data FrameRate = Hz Int

type PosX      = Double
type PosY      = Double
data Position  = Pos { posX :: PosX, posY :: PosY } deriving (Eq, Show)
data Velocity  = Vel { velX :: Double, velY :: Double } deriving (Eq, Show)

animate :: MonadIO m => state -> (state -> Interval -> state) -> Event Interval -> m (Behavior state)
animate start anim frames = 
  accumB start (flip anim <$> frames)

fromFrameRate :: FrameRate -> Interval
fromFrameRate (Hz fr) = Ms $ 1000 `div` fr

fromInterval :: Interval -> FrameRate
fromInterval (Ms ms)
  | ms >= 1   = Hz $ 1000 `div` ms
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
type Rect      = (Position, Double, Double)

data Ball      = Ball { ballPos   :: Position
                      , ballVel   :: Velocity
                      }

data Brick     = Brick { brickPos :: Position } deriving (Eq)

data GameStatus = Running | Lost
  deriving (Eq)

data GameState = GS { paddleX :: PosX
                    , ball    :: Ball 
                    , bricks  :: [Brick]
                    , status  :: GameStatus
                    }

data Size  = Size  { szWidth :: Int
                   , szHeight :: Int }

data World = World { screenSize   :: Size
                   , mouseX       :: Event Double
                   , drawCanvas   :: C.Canvas
                   , screenCanvas :: C.Canvas
                   , toScreen     :: UI ()
                   }

initBricks :: World -> [Brick]
initBricks world = [ brickAt x y | x <- xs, y <- ys]
  where brickAt px py = createBrickAt (Pos px py)
        xs            = [ 100 + fromIntegral i * (brickWidth + sep) | i <- [0 .. bricksInRow-1] ]
        ys            = [ 100 + fromIntegral i * (brickHeight + sep) | i <- [0..rows-1] ]
        bricksInRow   = floor $ (screenWidth- 190) / (brickWidth + sep)
        screenWidth   = fromIntegral . szWidth . screenSize $ world 
        rows          = 4
        sep           = 10


animateBall :: World -> Interval -> GameState -> GameState
animateBall w i (GS pdlX ball bricks Running) = hitBricks $ GS pdlX ball' bricks (checkStatus w ball')
  where ball' = reflectAtPaddle w pdlX . reflectAtWalls w . moveBall i $ ball
animateBall _ _ gs@(GS _ _ _ Lost)       = gs

moveBall :: Interval -> Ball -> Ball
moveBall (Ms ms) (Ball (Pos px py) v@(Vel vx vy)) = Ball (Pos (px + dt * vx) (py + dt * vy)) v
  where dt = if ms /= 0 then fromIntegral ms / 1000 else 0

hitBricks :: GameState -> GameState
hitBricks gs = gs { bricks = notHit, ball = ball' }
  where notHit   = bricks gs \\ (if null hits then [] else map fst hits)
        ball'    = if null hits then (ball gs) else (snd . head $ hits)
        hits     = catMaybes . map (collideWithBrick $ ball gs) $ (bricks gs)

reflectAtPaddle :: World -> PosX -> Ball -> Ball
reflectAtPaddle w x b@(Ball pos vel@(Vel vx vy)) = 
  if posInPaddle w x pos then (Ball pos' vel') else b
  where vel' = Vel vx (negate vy)
        pos' = Pos (posX pos) (paddleTop w)

reflectAtWalls :: World -> Ball -> Ball
reflectAtWalls w (Ball (Pos px py) (Vel vx vy)) = Ball (Pos px' py') (Vel vx' vy')
  where (px', vx') = if px < 0 then (0, negate vx) else if px > width then (width, negate vx) else (px , vx)
        (py', vy') = if py < 0 then (0, negate vy) else if py > height then (height, negate vy) else (py , vy)
        (width, height) = ((fromIntegral . szWidth . screenSize $ w), (fromIntegral . szHeight . screenSize $ w))

checkStatus :: World -> Ball -> GameStatus
checkStatus world (Ball (Pos px py) _) =
  if py > paddleBottom world then Lost else Running

paddleWidth :: Double
paddleWidth = 50

paddleHeight :: Double
paddleHeight = 10

paddleTop :: World -> Double
paddleTop world = fromIntegral (szHeight . screenSize $ world) - 2*paddleHeight

paddleBottom :: World -> Double
paddleBottom world = paddleTop world + paddleHeight

paddleLeft :: PosX -> Double
paddleLeft x = x - (paddleWidth / 2)

paddleRight :: PosX -> Double
paddleRight x = paddleLeft x + paddleWidth

paddleRect :: World -> PosX -> Rect
paddleRect w x = (Pos (paddleLeft x) (paddleTop w), paddleWidth, paddleHeight)

posInPaddle :: World -> PosX ->  Position -> Bool
posInPaddle world x = posInRect (paddleRect world x)

createBrickAt :: Position -> Brick
createBrickAt pos = Brick pos

brickWidth :: Double
brickWidth = 30

brickHeight :: Double
brickHeight = 10

brickRect :: Brick -> Rect
brickRect b = (brickPos b, brickWidth, brickHeight)

collideWithBrick :: Ball -> Brick -> Maybe (Brick, Ball)
collideWithBrick ball brick =
    case (nearHor, nearVer) of
      (False, False) -> Nothing
      (True,  False) -> Just (brick, reflHor ball)
      (False,  True) -> Just (brick, reflVer ball)
      (True,   True) -> Just (brick, (reflVer . reflHor $ ball))
    where nearHor         = (left <= px && px <= left+width) && (py-4 <= top && top <= py+4 || py-4 <= top+height && top+height <= py+4)
          nearVer         = (top <= py && py <= top+height) && (px-4 <= left && left <= px+4 || px-4 <= left+width && left+width <= px+4)
          reflHor (Ball b (Vel vx vy)) = Ball b (Vel vx (negate vy))
          reflVer (Ball b (Vel vx vy)) = Ball b (Vel (negate vx) vy)
          (Pos px py)     = ballPos ball
          (Pos left top)  = brickPos brick
          (width, height) = (brickWidth, brickHeight)


posInRect :: Rect -> Position -> Bool
posInRect ((Pos left top), width, height) (Pos px py) = 
  (left <= px && px <= left+width) && (top <= py && py <= top+height)

{-----------------------------------------------------------------------------
    View
------------------------------------------------------------------------------}

mkWorld :: Size -> UI World
mkWorld screenSize = do
    dc <- mkCanvas screenSize
    sc <- mkCanvas screenSize

    -- paddlePos <- stepper (szWidth sz `div` 2) $ (fst <$> Ev.mousemove canvas)
    let mouseX = fromIntegral . fst <$> E.mousemove sc
    let sw = C.drawImage dc (0,0) sc
    return $ World screenSize mouseX dc sc sw

updateCanvas :: World -> GameState -> UI ()
updateCanvas world (GS p ball bricks status) = do
    let c  = drawCanvas world
        sz = screenSize world
        setFill fs = 
          return c # set C.fillStyle fs
        getPos (Brick (Pos x y)) = ((x,y), brickWidth, brickHeight)

    setFill (if status == Lost then lost else white)
    C.fillRect (0, 0) (fromIntegral . szWidth $ sz) (fromIntegral . szHeight $ sz) c

    setFill brick
    C.fillRects (map getPos bricks) c

    setFill fill
    C.fillRect (x0, y0) w h c

    setFill red
    C.fillRect (posX (ballPos ball) - 4, posY (ballPos ball) - 4) 8 8 c

    toScreen world

    where brick  = C.solidColor (C.RGB 0 0 255)
          fill   = C.createHorizontalLinearGradient (x0, y0) w  (C.RGB 255 10 10) (C.RGB 10 10 255)
          white  = C.solidColor (C.RGBA 255 255 255 0.55)
          red    = C.solidColor (C.RGB 255 0 0)
          lost   = C.solidColor (C.RGB 250 50 50)
          y0     = paddleTop world
          x0     = paddleLeft p
          w      = paddleWidth
          h      = paddleHeight

-- | Create a canvas
mkCanvas :: Size -> UI C.Canvas
mkCanvas sz = do
    canvas <- UI.canvas
        # set UI.height (szHeight sz)
        # set UI.width  (szWidth sz)
        # set A.id_ "myCanvas"
        # set style [("border", "solid black 1px")]

    return canvas    
