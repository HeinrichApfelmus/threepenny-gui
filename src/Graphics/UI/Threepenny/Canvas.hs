module Graphics.UI.Threepenny.Canvas (
    -- * Synopsis
    -- | Partial binding to the HTML5 canvas API.

    -- * Documentation
    Canvas
    , Vector, Point
    , Color, ColorStop, Gradient, Style
    , drawImage, clearCanvas
    , solidColor, linearGradient, horizontalLinearGradient, verticalLinearGradient
    , fillStyle, strokeStyle, lineWidth, textFont
    , TextAlign(..), textAlign
    , fill, stroke, fillText, strokeText
    , Drawing, DrawingPath, renderDrawing, closedPath, openedPath, line, path, bezierCurve, move
    , fillRect, arc, arc', scale, translate, rgbColor, rgbaColor
    ) where

import Data.Char (toUpper)
import Data.List(intercalate)
import Data.Monoid
import Numeric (showHex)

import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Internal.FFI
import qualified Data.Aeson as JSON

{-----------------------------------------------------------------------------
    Canvas
------------------------------------------------------------------------------}
type Canvas = Element
type Vector = Point
type Point  = (Double, Double)

data PointCoord 
    = PA (Double,Double) -- ^ This is a point in the absolute coordinate
    | PU (Double,Double) -- ^ This is a point in the unity coordiante [-1;1], [-1,1] 
data Color  = RGB  { red :: Int, green :: Int, blue :: Int }
            | RGBA { red :: Int, green :: Int, blue :: Int, alpha :: Double }
            deriving (Eq, Show)

type ColorStop = (Double,  Color)

data Gradient  
    -- | defines a linear gradient 
    -- see <http://www.w3schools.com/tags/canvas_createlineargradient.asp> 
    = LinearGradient 
      { upperLeft  :: Vector -- ^ the left-upper point where the gradient should begin
      , gradWidth  :: Double -- ^ the width of the gradient
      , gradHeight :: Double -- ^ the height of the gradient
      , colorStops :: [ColorStop] -- ^ the gradients color stops
      } deriving (Show, Eq)

data Style 
    = SolidColor Color
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
    Styles
------------------------------------------------------------------------------}
-- | Create a color from rgb
rgbColor :: Int -> Int -> Int -> Color
rgbColor r g b = RGB r g b

-- | Create a color from rgba
rgbaColor :: Int -> Int -> Int -> Double -> Color
rgbaColor r g b a = RGBA r g b a 

-- | creates a solid-color style
solidColor :: Color -> Style
solidColor rgb = SolidColor rgb

-- | creates a linear gradient fill style
linearGradient :: Point       -- ^ The upper-left coordinate of the gradient
               -> Double      -- ^ The width of the gradient
               -> Double      -- ^ The height of the gradient
               -> [ColorStop] -- ^ the color-stops for the gradient
               -> Style
linearGradient (x0, y0) w h sts = Gradient $ LinearGradient (x0,y0) w h sts

-- | creates a simple horizontal gradient
horizontalLinearGradient:: Point  -- ^ The upper-left coordinate of the gradient
                        -> Double -- ^ The width of the gradient
                        -> Color  -- ^ The starting color of the gradient
                        -> Color  -- ^ The ending color of the gradient
                        -> Style
horizontalLinearGradient pt w c0 c1 = linearGradient pt w 0 [(0, c0), (1, c1)]

-- | creates a simple vertical gradient
verticalLinearGradient:: Point  -- ^ The upper-left coordinate of the gradient
                      -> Double -- ^ The height of the gradient
                      -> Color  -- ^ The starting color of the gradient
                      -> Color  -- ^ The ending color of the gradient
                      -> Style
verticalLinearGradient pt h c0 c1 = linearGradient pt 0 h [(0, c0), (1, c1)]

{-----------------------------------------------------------------------------
    general
------------------------------------------------------------------------------}

-- | Clear the canvas
clearCanvas :: Canvas -> UI ()
clearCanvas = runFunction . ffi "%1.getContext('2d').clear()"

{-----------------------------------------------------------------------------
    Drawing
------------------------------------------------------------------------------}
newtype Drawing 
    -- | Describe how to draw on a canvas
    = Drawing { draw :: Canvas -> UI () }

newtype DrawingPath
    -- | Describe how to draw a path on a canvas.
    = DrawingPath { drawPath :: Canvas -> UI () }

instance Monoid DrawingPath where
    mappend (DrawingPath first) (DrawingPath second) = DrawingPath seq
        where 
            seq canvas = do
                first canvas
                second canvas
    mempty = DrawingPath emptyDraw
        where 
            emptyDraw canvas = return ()

instance Monoid Drawing where
    mappend (Drawing first) (Drawing second) = Drawing seq
        where 
            seq canvas = do
                first canvas
                second canvas
    mempty = Drawing emptyDraw
        where 
            emptyDraw canvas = return ()

-- | Low level pimitive to start a path
drawBeginPath :: Point -> DrawingPath
drawBeginPath start = DrawingPath startPath'
    where
        startPath' canvas = do
            beginPath canvas
            moveTo start canvas

-- | Low level primitive to join a line
drawLineTo :: Point -> DrawingPath
drawLineTo = DrawingPath . lineTo

-- | Draw a line
line :: Point -> Point -> DrawingPath
line start end = DrawingPath line'
    where
        line' canvas = do
            moveTo start canvas
            lineTo end canvas

-- | Scale subsequent drawing
scale :: Double -> Double -> DrawingPath
scale xscale yscale = DrawingPath $ scaleTo xscale yscale

-- | Translate subsequent drawing
translate :: Double -> Double -> DrawingPath
translate x y = DrawingPath $ translateTo x y

-- | Add arc to the current path
arc 
    :: Point    -- ^ Center of the circle of which the arc is a part.
    -> Double   -- ^ Radius of the circle of which the arc is a part.
    -> Double   -- ^ Starting angle, in radians.
    -> Double   -- ^ Ending angle, in radians.
    -> DrawingPath
arc center radius startAngle endAngle = DrawingPath $ addArc center radius startAngle endAngle


-- | Like 'arc', but with an extra argument that indicates whether
-- we go in counter-clockwise ('True') or clockwise ('False') direction.
arc'
    :: Point    -- ^ Center of the circle of which the arc is a part.
    -> Double   -- ^ Radius of the circle of which the arc is a part.
    -> Double   -- ^ Starting angle, in radians.
    -> Double   -- ^ Ending angle, in radians.
    -> Bool
    -> DrawingPath
arc' center radius startAngle endAngle anti = DrawingPath $ addArc' center radius startAngle endAngle anti

-- | Draw a path
-- 
-- The path is drawn following the list of point
path :: [Point] -> DrawingPath
path [] = mempty
path (first:points) = drawBeginPath first <> (mconcat $ fmap drawLineTo points)

-- | Draw a bwzier curve in the current path
bezierCurve :: [Point] -> DrawingPath
bezierCurve = DrawingPath . bezierCurveTo

-- | Draw a closed path
--
-- The path is drawn following the list of point and it is closed after the final point.
closedPath 
    :: Style        -- ^ Style to apply to the path
    -> Double       -- ^ Width of the path
    -> DrawingPath  -- ^ Path to draw
    -> Drawing
closedPath style width (DrawingPath draw) = 
    Drawing beginPath <>
    Drawing draw <> 
    Drawing closePath <> 
    setDraw lineWidth width <>
    setDraw strokeStyle style <>
    Drawing stroke

-- | Stop the drawing and move to a new location
move :: Point -> DrawingPath
move = DrawingPath . moveTo

-- | Draw a path
openedPath 
    :: Style
    -> Double
    -> DrawingPath
    -> Drawing
openedPath style width (DrawingPath draw) =
    Drawing beginPath <>
    Drawing draw <>
    setDraw lineWidth width <>
    setDraw strokeStyle style <>
    Drawing stroke

-- | Drawing a filled rectangle.
--
-- The attribute determines the color.
fillRect
    :: Point    -- ^ upper left corner
    -> Double   -- ^ width in pixels
    -> Double   -- ^ height in pixels
    -> Drawing
fillRect point width height = Drawing $ fillRect' point width height

-- | Render a drawing on a canvas
renderDrawing :: Canvas -> Drawing -> UI ()
renderDrawing canvas (Drawing draw) = do
    draw canvas 

-- | Set an attribute in the drawing canvas context
setDraw :: ReadWriteAttr Canvas i o -> i -> Drawing 
setDraw attr i = Drawing $ set' attr i 

{-----------------------------------------------------------------------------
    Primitives
------------------------------------------------------------------------------}
-- | Draw a filled rectangle.
--
-- The 'fillStyle' attribute determines the color.
fillRect'
    :: Point    -- ^ upper left corner
    -> Double   -- ^ width in pixels
    -> Double   -- ^ height in pixels
    -> Canvas -> UI ()
fillRect' (x,y) w h canvas =
  runFunction $ ffi "%1.getContext('2d').fillRect(%2, %3, %4, %5)" canvas x y w h

-- | The Fillstyle to use inside shapes.
-- write-only as I could not find how to consistently read the fillstyle
fillStyle :: WriteAttr Canvas Style
fillStyle = mkWriteAttr $ assignStyle "fillStyle"

-- | The color or style to use for the lines around shapes.
-- Default is @#000@ (black).
strokeStyle :: WriteAttr Canvas Style
strokeStyle = mkWriteAttr $ assignStyle "strokeStyle"

-- | The width of lines. Default is @1@.
lineWidth :: Attr Canvas Double
lineWidth = fromObjectProperty "getContext('2d').lineWidth"

-- | The font used for 'fillText' and 'strokeText'.
-- Default is @10px sans-serif@.
textFont :: Attr Canvas String
textFont = fromObjectProperty "getContext('2d').font"

-- | sets the current property' style of the canvas context
assignStyle :: String -> Style -> Canvas -> UI ()
assignStyle prop (Gradient fs) canvas =
    runFunction $ ffi cmd canvas
        where cmd = "var ctx=%1.getContext('2d'); var grd=" ++ fsStr fs ++ cStops fs ++ "ctx." ++ prop ++ "=grd;"
              fsStr (LinearGradient (x0, y0) w h _) 
                                                = "ctx.createLinearGradient(" ++ pStr [x0, y0, x0+w, y0+h] ++ ");"
              cStops (LinearGradient _ _ _ sts) = concatMap addStop sts
              addStop (p,c)                     = "grd.addColorStop(" ++ show p ++ ",'" ++ rgbString c ++ "');"
              pStr                              = intercalate "," . map show
assignStyle prop (SolidColor color) canvas =
    runFunction $ ffi ("%1.getContext('2d')." ++ prop ++ "=%2") canvas (rgbString color)

data TextAlign = Start | End | LeftAligned | RightAligned | Center
               deriving (Eq, Show, Read)

aToS :: TextAlign -> String
aToS algn =
  case algn of
    Start -> "start"
    End -> "end"
    LeftAligned -> "left"
    RightAligned -> "right"
    Center -> "center"

sToA :: String -> TextAlign
sToA algn =
  case algn of
    "start" -> Start
    "end" -> End
    "left" -> LeftAligned
    "right" -> RightAligned
    "center" -> Center
    _ -> Start

-- | The alignment for 'fillText' and 'strokeText'. Default is 'Start'.
textAlign :: Attr Canvas TextAlign
textAlign = bimapAttr aToS sToA $ textAlignStr
    where
    textAlignStr :: Attr Canvas String
    textAlignStr = fromObjectProperty "getContext('2d').textAlign"

-- | Starts a new path by resetting the list of sub-paths.
-- Call this function when you want to create a new path.
beginPath :: Canvas -> UI()
beginPath = runFunction . ffi "%1.getContext('2d').beginPath()"

-- | Moves the starting point of a new subpath to the @(x,y)@ coordinate.
moveTo :: Point -> Canvas -> UI()
moveTo (x,y) canvas =
  runFunction $ ffi "%1.getContext('2d').moveTo(%2, %3)" canvas x y

-- | Connects the last point in the subpath to the @(x,y)@ coordinates
-- with a straight line.
lineTo :: Point -> Canvas -> UI()
lineTo (x,y) canvas =
  runFunction $ ffi "%1.getContext('2d').lineTo(%2, %3)" canvas x y

-- | Draw a straight line from the current point to the start of the
-- path. If the shape has already been closed or has only one point,
-- this function does nothing.
closePath :: Canvas -> UI()
closePath = runFunction . ffi "%1.getContext('2d').closePath()"

-- | Add a circular arc to the current path.
addArc
    :: Point    -- ^ Center of the circle of which the arc is a part.
    -> Double   -- ^ Radius of the circle of which the arc is a part.
    -> Double   -- ^ Starting angle, in radians.
    -> Double   -- ^ Ending angle, in radians.
    -> Canvas -> UI ()
addArc (x,y) radius startAngle endAngle canvas =
    runFunction $ ffi "%1.getContext('2d').arc(%2, %3, %4, %5, %6)"
        canvas x y radius startAngle endAngle

-- | Like 'arc', but with an extra argument that indicates whether
-- we go in counter-clockwise ('True') or clockwise ('False') direction.
addArc' :: Point -> Double -> Double -> Double -> Bool -> Canvas -> UI ()
addArc' (x,y) radius startAngle endAngle anti canvas =
    runFunction $ ffi "%1.getContext('2d').arc(%2, %3, %4, %5, %6, %7)"
        canvas x y radius startAngle endAngle anti

-- | Add a bezier curve to the current path
bezierCurveTo :: [Point] -> Canvas -> UI ()
bezierCurveTo points canvas = 
    runFunction $ ffi "%1.getContext('2d').bezierCurveTo(%2)" canvas (VariadicJSParam (concat $ map flatenPoint  points))

-- | Helper function to convert point to variadic parameter for ffi.
flatenPoint :: Point -> [Double]
flatenPoint (x,y) = [x,y]

-- | Fills the subpaths with the current fill style.
fill :: Canvas -> UI ()
fill = runFunction . ffi "%1.getContext('2d').fill()"

-- | Strokes the subpaths with the current stroke style.
stroke :: Canvas -> UI ()
stroke = runFunction . ffi "%1.getContext('2d').stroke()"

-- | Render a text in solid color at a certain point on the canvas.
-- 
-- The 'fillStyle' attribute determines the color.
-- The 'textFont' attribute determines the font used.
-- The 'textAlign' attributes determines the position of the text
-- relative to the point.
fillText :: String -> Point -> Canvas -> UI ()
fillText text (x,y) canvas =
  runFunction $ ffi "%1.getContext('2d').fillText(%2, %3, %4)" canvas text x y

-- | Render the outline of a text at a certain point on the canvas.
-- 
-- The 'strokeStyle' attribute determines the color of the outline.
-- The 'textFont' attribute determines the font used.
-- The 'textAlign' attributes determines the position of the text
-- relative to the point.
strokeText :: String -> Point -> Canvas -> UI ()
strokeText text (x,y) canvas =
  runFunction $ ffi "%1.getContext('2d').strokeText(%2, %3, %4)" canvas text x y

-- | Scale subsequent drawing
scaleTo :: Double -> Double -> Canvas -> UI ()
scaleTo xscale yscale canvas = 
    runFunction $ ffi "%1.getContext('2d').scale(%2,%3)" canvas xscale yscale

-- | Translate all subsequent drawing
translateTo :: Double -> Double -> Canvas -> UI ()
translateTo x y canvas =
    runFunction $ ffi "%1.getContext('2d').translate(%2,%3)" canvas x y

resetTransform :: Canvas -> UI ()
resetTransform canvas = runFunction $ ffi "%1.getContext('2d').resetTransform()" canvas
{-----------------------------------------------------------------------------
    helper functions
------------------------------------------------------------------------------}

rgbString :: Color -> String
rgbString color =
  case color of
    (RGB r g b) -> "#" ++ sh r ++ sh g ++ sh b
    (RGBA r g b a) -> "rgba(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ "," ++ show a ++ ")"
    where sh i  = pad . map toUpper $ showHex i ""
          pad s
            | length s  == 0 = "00"
            | length s  == 1 = '0' : s
            | length s  == 2 = s
            | otherwise      =  take 2 s
