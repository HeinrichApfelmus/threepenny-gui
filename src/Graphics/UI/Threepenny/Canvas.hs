module Graphics.UI.Threepenny.Canvas (
    -- * Synopsis
    -- | Partial binding to the HTML5 canvas API.
    
    -- * Documentation
    Canvas,
    Vector, drawImage, clearCanvas,
    RGB(..), ColorStop, Gradient, FillStyle, 
    solidColor, 
    createLinearGradient, createHorizontalLinearGradient, createVerticalLinearGradient,
    createRadialGradient,
    setFillStyle,
    Rect (..), fillRect, fillRectWith
    ) where

import Control.Monad (forM_)
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

type ColorStop = (Int,  RGB)

data Gradient  
    -- | defines a linear gradient 
    --   params are (x-coord start, y-coord start, x-coord end, y-coord end) 
    --   see <http://www.w3schools.com/tags/canvas_createlineargradient.asp>
    = LinearGradient Int Int Int Int [ColorStop]
    -- | defines a radial gradient 
    --   params are (x-coord start, y-coord start, radius starting circle, x-coord end, y-coord end, radius ending circle) 
    --   see <http://www.w3schools.com/tags/canvas_createradialgradient.asp>
    | RadialGradient Int Int Int Int Int Int [ColorStop]
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
                     -> Int -- ^ The x-coordinate of the end point of the gradient
                     -> Int -- ^ The y-coordinate of the end point of the gradient
                     -> [ColorStop] -- ^ the color-stops for the gradient
                     -> FillStyle
createLinearGradient x0 y0 x1 y1 sts = Gradient $ LinearGradient x0 y0 x1 y1 sts

-- | creates a simple horizontal gradient
createHorizontalLinearGradient:: Int -- ^ the width of the gradient
                              -> RGB -- ^ The starting color of the gradient
                              -> RGB -- ^ The ending color of the gradient
                              -> FillStyle
createHorizontalLinearGradient w c0 c1 = createLinearGradient 0 0 w 0 [(0, c0), (1, c1)]

-- | creates a simple vertical gradient
createVerticalLinearGradient:: Int -- ^ the height of the gradient
                            -> RGB -- ^ The starting color of the gradient
                            -> RGB -- ^ The ending color of the gradient
                            -> FillStyle
createVerticalLinearGradient h c0 c1 = createLinearGradient 0 0 0 h [(0, c0), (1, c1)]


-- | creates a radial gradient fill style
createRadialGradient :: Int -- ^ The x-coordinate of the start point of the gradient
                     -> Int -- ^ The y-coordinate of the start point of the gradient
                     -> Int -- ^ The radius of the starting circle
                     -> Int -- ^ The x-coordinate of the end point of the gradient
                     -> Int -- ^ The y-coordinate of the end point of the gradient
                     -> Int -- ^ the radius of the ending circle
                     -> [ColorStop] -- ^ the color-stops for the gradient
                     -> FillStyle
createRadialGradient x0 y0 r0 x1 y1 r1 sts = Gradient $ RadialGradient x0 y0 r0 x1 y1 r1 sts

-- | sets the current fill style of the canvas context
setFillStyle :: FillStyle -> Canvas -> UI ()
setFillStyle (Gradient fs) canvas = do
    runFunction $ ffi cmd canvas
    forM_ (cStops fs) $ addColorStop canvas
        where cmd = "%1.getContext('2d').fillStyle=%1.getContext('2d')." ++ fsStr fs
              fsStr (LinearGradient x0 y0 x1 y1 _)         = "createLinearGradient(" ++ pStr [x0, y0, x1, y1] ++ ")"
              fsStr (RadialGradient x0 y0 r0 x1 y1 r1 _ )  = "createRadialGradient(" ++ pStr [x0, y0, r0, x1, y1, r1] ++ ")"
              cStops (LinearGradient _ _ _ _ sts)          = sts
              cStops (RadialGradient _ _ _ _ _ _ sts)      = sts
              pStr                                         = intercalate "," . map show
setFillStyle (SolidColor color) canvas =
    runFunction $ ffi "%1.getContext('2d').fillStyle=%2" canvas (rgbString color)

-- | adds an color stop to the current fillStyle
addColorStop :: Canvas -> ColorStop -> UI()
addColorStop canvas (p, rgb) = 
    runFunction $ ffi cmd canvas (rgbString rgb)
        where cmd = "%1.getContext('2d').fillStyle.addColorStop(" ++ show p ++ ", %2)"


{-----------------------------------------------------------------------------
    fill primitives
------------------------------------------------------------------------------}

-- | fills a rectangle with the current fillStyle
fillRect :: Rect -> Canvas -> UI ()
fillRect rect canvas =
    runFunction $ ffi "%1.getContext('2d').fillRect(%2,%3,%4,%5)" canvas (rectLeft rect) (rectTop rect) (rectWidth rect) (rectHeight rect)

-- | fills a rectangle with a solid color (the current fillStyle will be set to that color!)
fillRectWith :: FillStyle -> Rect -> Canvas -> UI ()
fillRectWith (Gradient fs) rect canvas =
    runFunction $ ffi cmd canvas (rectLeft rect) (rectTop rect) (rectWidth rect) (rectHeight rect)
        where cmd = "var ctx=%1.getContext('2d'); var fs=" ++ fsStr fs ++ "; " ++ stStr fs ++"; ctx.fillStyle=fs; ctx.fillRect(%2,%3,%4,%5)"
              fsStr (LinearGradient x0 y0 x1 y1 _)         = "ctx.createLinearGradient(" ++ pStr [x0, y0, x1, y1] ++ ")"
              fsStr (RadialGradient x0 y0 r0 x1 y1 r1 _ )  = "ctx.createRadialGradient(" ++ pStr [x0, y0, r0, x1, y1, r1] ++ ")"
              stStr (LinearGradient _ _ _ _ sts)           = intercalate "; " . map csStr $ sts
              stStr (RadialGradient _ _ _ _ _ _  sts)      = intercalate "; " . map csStr $ sts
              pStr                                         = intercalate "," . map show
              csStr (p, rgb)                               = "fs.addColorStop(" ++ show p ++ ", '" ++ rgbString rgb ++ "')"
fillRectWith (SolidColor color) rect canvas =
    runFunction $ ffi cmd canvas (rgbString color) (rectLeft rect) (rectTop rect) (rectWidth rect) (rectHeight rect)
        where cmd = "var ctx=%1.getContext('2d'); ctx.fillStyle=%2; ctx.fillRect(%3,%4,%5,%6)"

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