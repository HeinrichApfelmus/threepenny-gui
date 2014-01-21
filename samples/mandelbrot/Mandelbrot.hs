module Mandelbrot 
    ( Compl (..)
    , ViewWindow (..)
    , PictureCoords (..)
    , PictureSize (..)
    , Steps
    , createImage
    , createImageParallel
    , project
    , zoomTo
    ) where

import Codec.Picture (PixelRGB8(..), Image, generateImage, writePng)
import Data.Array.Repa as Repa hiding ((++))
import Data.Functor.Identity

data Compl =
    C { re :: Double
      , im :: Double
      } deriving (Eq)

data ViewWindow =
    View { upperLeft  :: Compl
         , lowerRight :: Compl
         } deriving (Show)

data PictureCoords = Coords { x :: Int, y :: Int }
data PictureSize   = Size   { width :: Int, height :: Int }

type Steps = Int
type RGB   = (Int, Int, Int)

createImageParallel :: ViewWindow -> Steps -> PictureSize -> String -> IO()
createImageParallel view maxSteps resolution imagePath = do
  let image = mandelbrotParallel maxSteps view resolution
  writePng imagePath image

createImage :: ViewWindow -> Steps -> PictureSize -> String -> IO()
createImage view maxSteps resolution imagePath = do
  let image = mandelbrotImage maxSteps view resolution
  writePng imagePath image

-- | calculates the mandelbrot-set as an colored image
mandelbrotImage :: Steps -> ViewWindow -> PictureSize -> Image PixelRGB8
mandelbrotImage maxSteps vw sz@(Size w h) = generateImage calcPixel w h
    where calcPixel x' y' = color maxSteps $ mandelbrotIter maxSteps $ project vw sz (Coords x' y')

-- | calculates the mandelbrot-set as an colored image using the Repa package (parallel array computation)
mandelbrotParallel :: Steps -> ViewWindow -> PictureSize -> Image PixelRGB8
mandelbrotParallel maxSteps vw sz@(Size w h) = runIdentity go
    where go             = do
            arr <- stepArray
            return $ generateImage (getPixel arr) w h
          getPixel arr x' y' = rgbToPixelRGB8 $ arr ! (Z:.x':.y')
          stepArray :: Identity (Array U DIM2 RGB)
          stepArray          = computeP $ mandelbrotArray maxSteps vw sz

mandelbrotArray :: Steps -> ViewWindow -> PictureSize -> Array D DIM2 RGB
mandelbrotArray maxSteps vw sz@(Size w h) = fromFunction (Z:.w:.h) calcPixel
    where calcPixel (Z:.x':.y') = colorRGB maxSteps $ mandelbrotIter maxSteps $ project vw sz (Coords x' y')

color :: Steps -> Steps -> PixelRGB8
color maxSteps = rgbToPixelRGB8 . colorRGB maxSteps

rgbToPixelRGB8 :: RGB -> PixelRGB8
rgbToPixelRGB8 (r, g, b) = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)

-- | translates the steps taken till the sequence got out-of-bounds into a RGB value for a color using a very basic approach of just bit-masking (notice: in the later stages the PixelRGB8 structure will "mod" to 8-bits by default)
colorRGB :: Steps -> Steps -> RGB
colorRGB maxSteps steps
    | steps >= maxSteps = (0, 0, 0)
    | otherwise         = (sr, sg, sb)
    where sr = steps
          sg = 64 * (steps `div` 256)
          sb = 16 * (steps `div` 65536)


-- | translates a picture-coords into the ViewWindow
project :: ViewWindow -> PictureSize -> PictureCoords -> Compl
project (View ul lr) sz c = ul + (C (w*x') (h*y'))
    where (C w h) = lr - ul
          x'      = fromIntegral (x c) / fromIntegral (width sz)
          y'      = fromIntegral (y c) / fromIntegral (height sz)

viewWidth :: ViewWindow -> Double
viewWidth v = re $ lowerRight v - upperLeft v

viewHeight :: ViewWindow -> Double
viewHeight v = im $ lowerRight v - upperLeft v

zoomTo :: Double -> Compl -> ViewWindow -> ViewWindow
zoomTo z c v = View (c-d) (c+d)
    where d = C (w/2) (h/2)
          w = (1/z) * viewWidth v
          h = (1/z) * viewHeight v

-- | processes - based on a maximum step count and a starting point -
--   the numbers or steps it takes a point using the iteration-rule
--   z' = z*z + c - to escape the bound region
mandelbrotIter :: Steps -> Compl -> Steps
mandelbrotIter maxSteps c = runIter 0 c
    where runIter steps z =
            if steps >= maxSteps || escapes z
                then steps
                else runIter (steps+1) (iter z)
          iter z = z*z + c

-- | does a point in the complex plane escape to infinity
--   (of course using mandelbrots iteration rule z' = z*z + c)
escapes :: Compl -> Bool
escapes = (>= 4) . len2

len2 :: Compl -> Double
len2 (C r i) = r*r + i*i

-- | Implement Compl as Num represented as a complex number in the
--   most obvious way - notice that signum was choosen to fullfill
--   abs x * signum x == x
instance Num Compl where
    fromInteger i = C (fromInteger i) 0
    (C r i) + (C r' i') = C (r+r') (i+i')
    (C r i) - (C r' i') = C (r-r') (i-i')
    (C r i) * (C r' i') = C (r*r' - i*i') (r*i' + i*r')
    negate (C r i)      = C (-r) (-i)
    abs c               = C (sqrt $ len2 c) 0
    signum c@(C r i)    = C (r / l2) (i / l2)
        where l2        = len2 c

instance Show Compl where 
  show (C r i) = show r ++ " + " ++ show i ++ "I"