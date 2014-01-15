module Graphics.UI.Threepenny.Canvas (
    -- * Synopsis
    -- | Partial binding to the HTML5 canvas API.

    -- * Documentation
    Canvas,
    Vector, drawImage, clearCanvas
    , fillRect, fillStyle, strokeStyle, lineWidth, textFont
    , beginPath, moveTo, lineTo, closePath, arc, arc'
    , fill, stroke, fillText, strokeText
    ) where

import Graphics.UI.Threepenny.Core
import qualified Data.Aeson as JSON

{-----------------------------------------------------------------------------
    Canvas
------------------------------------------------------------------------------}
type Canvas = Element

type Vector = (Int,Int)

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


{-
-- Would have liked to make this function for all the attributes,
-- alas I cannot convince Haskell's type system that it is OK

mkContextAttr :: String -> ReadWriteAttr Canvas a a
mkContextAttr attr = mkReadWriteAttr getter setter
  where
    setter value canvas =
      runFunction $ ffi ("%1.getContext('2d')."++ attr ++ "= %2") canvas value
    getter =
      callFunction . ffi $ "%1.getContext('2d')." ++ attr
-}

-- | The color or style to use inside shapes. Default is @#000@ (black).
fillStyle :: ReadWriteAttr Canvas String String
fillStyle = mkReadWriteAttr getter setter
  where
    setter value canvas =
      runFunction $ ffi "%1.getContext('2d').fillStyle = %2" canvas value
    getter =
      callFunction . ffi "%1.getContext('2d').fillStyle"

-- | The color or style to use for the lines around shapes.
-- Default is @#000@ (black).
strokeStyle :: ReadWriteAttr Canvas String String
strokeStyle = mkReadWriteAttr getter setter
  where
    setter value canvas =
      runFunction $ ffi "%1.getContext('2d').strokeStyle = %2" canvas value
    getter =
      callFunction . ffi "%1.getContext('2d').strokeStyle"

-- | The width of lines. Default is @1@
lineWidth :: ReadWriteAttr Canvas Double Double
lineWidth = mkReadWriteAttr getter setter
  where
    setter value canvas =
      runFunction $ ffi "%1.getContext('2d').lineWidth = %2" canvas (JSON.toJSON value)
    getter =
      fmap read . callFunction . ffi "%1.getContext('2d').lineWidth"

-- | The font used for 'fillText' and 'strokeText'. Default is @10px sans-serif@.
textFont :: ReadWriteAttr Canvas String String
textFont = mkReadWriteAttr getter setter
  where
    setter value canvas =
      runFunction $ ffi "%1.getContext('2d').font = %2" canvas value
    getter =
      callFunction . ffi "%1.getContext('2d').font"


-- | Starts a new path by resetting the list of sub-paths. Call this function when you want to create a new path.
beginPath :: Canvas -> UI()
beginPath = runFunction . ffi "%1.getContext('2d').beginPath()"

-- | Moves the starting point of a new subpath to the (x, y) coordinate.
moveTo :: Vector -> Canvas -> UI()
moveTo (x,y) canvas =
  runFunction $ ffi "%1.getContext('2d').moveTo(%2, %3)" canvas x y

-- | Connects the last point in the subpath to the (x, y) coordinates
-- with a straight line.
lineTo :: Vector -> Canvas -> UI()
lineTo (x,y) canvas =
  runFunction $ ffi "%1.getContext('2d').lineTo(%2, %3)" canvas x y

-- | Draw a straight line from the current point to the start of the
-- path. If the shape has already been closed or has only one point,
-- this function does nothing.
closePath :: Canvas -> UI()
closePath = runFunction . ffi "%1.getContext('2d').closePath()"

-- | @arc (x,y) radius startAngle endAngle canvas@ adds an arc to the
-- path which is centered at @(x, y)@ coordinates with radius @r@
-- starting at @startAngle@ and ending at @endAngle@ going in
-- clockwise direction. Angles are given as radians.
arc :: Vector -> Int -> Double -> Double -> Canvas -> UI ()
arc (x,y) radius startAngle endAngle canvas =
  runFunction $ ffi "%1.getContext('2d').arc(%2, %3, %4, %5, %6)" canvas x y radius
                                      (JSON.toJSON startAngle) (JSON.toJSON endAngle)

-- | Like 'arc', but with an extra argument for going in anticlockwise direction
arc' :: Vector -> Int -> Double -> Double -> Bool -> Canvas -> UI ()
arc' (x,y) radius startAngle endAngle anti canvas =
  runFunction $ ffi "%1.getContext('2d').arc(%2, %3, %4, %5, %6, %7)" canvas x y
                               radius (JSON.toJSON startAngle) (JSON.toJSON endAngle)
                               anti

-- | Fills the subpaths with the current fill style.
fill :: Canvas -> UI ()
fill = runFunction . ffi "%1.getContext('2d').fill()"

-- | Strokes the subpaths with the current stroke style.
stroke :: Canvas -> UI ()
stroke = runFunction . ffi "%1.getContext('2d').stroke()"

-- | @fillText t (x, y) c@ renders the text @t@ with in solid color to the
-- canvas @c@ at @(x, y)@ coordinates. The color used is the one set
-- by 'setFillStyle' and the font use is the one set by 'setFont'.
fillText :: String -> Vector -> Canvas -> UI ()
fillText text (x,y) canvas =
  runFunction $ ffi "%1.getContext('2d').fillText(%2, %3, %4)" canvas text x y

-- | @strokeText t (x, y) c@ renders the outline of text @t@ to the
-- canvas @c@ at @(x, y)@ coordinates. The color used is the one set
-- by 'setStrokeStyle' and the font used is the one set by 'setFont'.
strokeText :: String -> Vector -> Canvas -> UI ()
strokeText text (x,y) canvas =
  runFunction $ ffi "%1.getContext('2d').strokeText(%2, %3, %4)" canvas text x y
