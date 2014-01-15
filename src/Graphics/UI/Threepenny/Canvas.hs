module Graphics.UI.Threepenny.Canvas (
    -- * Synopsis
    -- | Partial binding to the HTML5 canvas API.

    -- * Documentation
    Canvas
    , Vector, Point, drawImage, clearCanvas
    , fillRect, fillStyle, strokeStyle, lineWidth, textFont
    , TextAlign(..), textAlign
    , beginPath, moveTo, lineTo, closePath, arc, arc'
    , fill, stroke, fillText, strokeText
    ) where

import Graphics.UI.Threepenny.Core
import qualified Data.Aeson as JSON

{-----------------------------------------------------------------------------
    Canvas
------------------------------------------------------------------------------}
type Canvas = Element

type Vector = Point
type Point  = (Double, Double)

-- | Draw the image of an image element onto the canvas at a specified position.
drawImage :: Element -> Vector -> Canvas -> UI ()
drawImage image (x,y) canvas =
    runFunction $ ffi "%1.getContext('2d').drawImage(%2,%3,%4)" canvas image x y

-- | Clear the canvas
clearCanvas :: Canvas -> UI ()
clearCanvas = runFunction . ffi "%1.getContext('2d').clear()"

-- | @fillRect x y w h canvas@ draw a filled rectangle (using the
-- current fillStyle) at position @(x, y)@ with width @w@ and height
-- @h@.
fillRect :: Int -> Int -> Int -> Int -> Canvas -> UI()
fillRect x y w h canvas =
  runFunction $ ffi "%1.getContext('2d').fillRect(%2, %3, %4, %5)" canvas x y w h

-- | The color or style to use inside shapes.
-- Default is @#000@ (black).
fillStyle :: Attr Canvas String
fillStyle = fromObjectProperty "getContext('2d').fillStyle"

-- | The color or style to use for the lines around shapes.
-- Default is @#000@ (black).
strokeStyle :: Attr Canvas String
strokeStyle = fromObjectProperty "getContext('2d').strokeStyle"

-- | The width of lines. Default is @1@.
lineWidth :: Attr Canvas Double
lineWidth = fromObjectProperty "getContext('2d').lineWidth"

-- | The font used for 'fillText' and 'strokeText'.
-- Default is @10px sans-serif@.
textFont :: Attr Canvas String
textFont = fromObjectProperty "getContext('2d').font"

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

-- | The alignment for 'fillText' and 'strokeText'. Default is @Start@.
textAlign :: ReadWriteAttr Canvas TextAlign TextAlign
textAlign = bimapAttr aToS sToA $ textAlignStr
    where
    textAlignStr :: Attr Canvas String
    textAlignStr = fromObjectProperty "getContext('2d').textAlign"

-- | Starts a new path by resetting the list of sub-paths. Call this function when you want to create a new path.
beginPath :: Canvas -> UI()
beginPath = runFunction . ffi "%1.getContext('2d').beginPath()"

-- | Moves the starting point of a new subpath to the (x, y) coordinate.
moveTo :: Point -> Canvas -> UI()
moveTo (x,y) canvas =
  runFunction $ ffi "%1.getContext('2d').moveTo(%2, %3)" canvas x y

-- | Connects the last point in the subpath to the (x, y) coordinates
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
arc
    :: Point    -- ^ Center of the circle of which the arc is a part.
    -> Double   -- ^ Radius of the circle of which the arc is a part.
    -> Double   -- ^ Starting angle, in radians).
    -> Double   -- ^ Ending angle, in radians.
    -> Canvas -> UI ()
arc (x,y) radius startAngle endAngle canvas =
    runFunction $ ffi "%1.getContext('2d').arc(%2, %3, %4, %5, %6)"
        canvas x y radius startAngle endAngle

-- | Like 'arc', but with an extra argument that indicates whether
-- we go in counter-clockwise ('True') or clockwise ('False') direction.
arc' :: Point -> Double -> Double -> Double -> Bool -> Canvas -> UI ()
arc' (x,y) radius startAngle endAngle anti canvas =
    runFunction $ ffi "%1.getContext('2d').arc(%2, %3, %4, %5, %6, %7)"
        canvas x y radius startAngle endAngle anti

-- | Fills the subpaths with the current fill style.
fill :: Canvas -> UI ()
fill = runFunction . ffi "%1.getContext('2d').fill()"

-- | Strokes the subpaths with the current stroke style.
stroke :: Canvas -> UI ()
stroke = runFunction . ffi "%1.getContext('2d').stroke()"

-- | @fillText t (x, y) c@ renders the text @t@ with in solid color to the
-- canvas @c@ at @(x, y)@ coordinates. The color used is the one set
-- by 'setFillStyle' and the font use is the one set by 'setFont'.
fillText :: String -> Point -> Canvas -> UI ()
fillText text (x,y) canvas =
  runFunction $ ffi "%1.getContext('2d').fillText(%2, %3, %4)" canvas text x y

-- | @strokeText t (x, y) c@ renders the outline of text @t@ to the
-- canvas @c@ at @(x, y)@ coordinates. The color used is the one set
-- by 'setStrokeStyle' and the font used is the one set by 'setFont'.
strokeText :: String -> Point -> Canvas -> UI ()
strokeText text (x,y) canvas =
  runFunction $ ffi "%1.getContext('2d').strokeText(%2, %3, %4)" canvas text x y
