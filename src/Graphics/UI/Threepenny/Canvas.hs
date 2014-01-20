module Graphics.UI.Threepenny.Canvas (
    -- * Synopsis
    -- | Partial binding to the HTML5 canvas API.
    
    -- * Documentation
    Canvas,
    Vector, drawImage, clearCanvas,
    RGB(..), ColorStop, Gradient, FillStyle, 
    solidColor, 
    createLinearGradient, createHorizontalLinearGradient, createVerticalLinearGradient,
    Rect (..), fillRect
    ) where

import Data.Char (toUpper)
import Data.List(intercalate)
import Numeric (showHex)

import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Canvas
------------------------------------------------------------------------------}
type Canvas = Element

type Vector = (Int,Int)
data Rect   = Rect { rectLeft :: Int, rectTop :: Int, rectWidth :: Int, rectHeight :: Int } deriving (Eq, Show)
data RGB    = RGB  {red :: Int, green :: Int,  blue :: Int } deriving (Eq, Show)

type ColorStop = (Double,  RGB)

data Gradient  
    -- | defines a linear gradient 
    --   params are the direction of the gradient and the colorstops on it's way
    --   the region as defined in <http://www.w3schools.com/tags/canvas_createlineargradient.asp> is calculated based on the
    --   output Rectangle
    = LinearGradient Vector [ColorStop]
    deriving (Show, Eq)

data FillStyle 
    = SolidColor RGB
    | Gradient Gradient
    deriving (Show, Eq) 


{-----------------------------------------------------------------------------
    Image drawing
------------------------------------------------------------------------------}

-- | Draw the image of an image element onto the canvas at a specified position.
drawImage :: Element -> Vector -> Canvas -> UI ()
drawImage image (x,y) canvas =
    runFunction $ ffi "%1.getContext('2d').drawImage(%2,%3,%4)" canvas image x y

{-----------------------------------------------------------------------------
    Fill Styles
------------------------------------------------------------------------------}

-- | creates a solid-color fillstyle
solidColor :: RGB -> FillStyle
solidColor rgb = SolidColor rgb

-- | creates a linear gradient fill style
createLinearGradient :: Int -- ^ The x-coordinate of the start point of the gradient
                     -> Int -- ^ The y-coordinate of the start point of the gradient
                     -> [ColorStop] -- ^ the color-stops for the gradient
                     -> FillStyle
createLinearGradient x0 y0 sts = Gradient $ LinearGradient (x0,y0) sts

-- | creates a simple horizontal gradient
createHorizontalLinearGradient:: RGB -- ^ The starting color of the gradient
                              -> RGB -- ^ The ending color of the gradient
                              -> FillStyle
createHorizontalLinearGradient c0 c1 = createLinearGradient 1 0 [(0, c0), (1, c1)]

-- | creates a simple vertical gradient
createVerticalLinearGradient:: RGB -- ^ The starting color of the gradient
                            -> RGB -- ^ The ending color of the gradient
                            -> FillStyle
createVerticalLinearGradient c0 c1 = createLinearGradient 0 1 [(0, c0), (1, c1)]

-- | sets the current fill style of the canvas context
assignFillStyle :: Rect -> FillStyle -> Canvas -> UI ()
assignFillStyle rect (Gradient fs) canvas =
    runFunction $ ffi cmd canvas
        where cmd = "var ctx=%1.getContext('2d'); var grd=" ++ fsStr fs ++ cStops fs ++ "ctx.fillStyle=grd;"
              fsStr (LinearGradient (dx, dy) _) = "ctx.createLinearGradient(" ++ pStr [x0, y0, x0+w, y0+h] ++ ");"
                where (x0, y0)                  = (rectLeft rect, rectTop rect)
                      (w, h)                    = (floor $ fx * fromIntegral (rectWidth rect), floor $ fy * fromIntegral (rectHeight rect))
                      (fx, fy)                  = (fromIntegral dx / m, fromIntegral dy / m)
                      m                         = max (fromIntegral dx) (fromIntegral dy)
              cStops (LinearGradient (_,_) sts) = concatMap addStop sts
              addStop (p,c)                     = "grd.addColorStop(" ++ show p ++ ",'" ++ rgbString c ++ "');"
              pStr                              = intercalate "," . map show
assignFillStyle _ (SolidColor color) canvas =
    runFunction $ ffi "%1.getContext('2d').fillStyle=%2" canvas (rgbString color)

{-----------------------------------------------------------------------------
    fill primitives
------------------------------------------------------------------------------}

-- | fills a rectangle
fillRect :: Rect -> FillStyle -> Canvas -> UI ()
fillRect rect fs canvas = do
    assignFillStyle rect fs canvas
    runFunction $ ffi "%1.getContext('2d').fillRect(%2,%3,%4,%5)" canvas (rectLeft rect) (rectTop rect) (rectWidth rect) (rectHeight rect)

{-----------------------------------------------------------------------------
    general
------------------------------------------------------------------------------}

-- | Clear the canvas
clearCanvas :: Canvas -> UI ()
clearCanvas = runFunction . ffi "%1.getContext('2d').clear()"

{-----------------------------------------------------------------------------
    helper functions
------------------------------------------------------------------------------}

rgbString :: RGB -> String
rgbString (RGB r g b) = "#" ++ sh r ++ sh g ++ sh b
    where sh i  = pad . map toUpper $ showHex i ""
          pad s
            | length s  == 0 = "00"
            | length s  == 1 = '0' : s
            | length s  == 2 = s
            | otherwise      =  take 2 s