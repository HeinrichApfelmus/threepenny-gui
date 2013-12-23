module Graphics.UI.Threepenny.Canvas (
    -- * Synopsis
    -- | Partial binding to the HTML5 canvas API.

    -- * Documentation
    Canvas,
    Vector, drawImage, clearCanvas
    , fillRect, setFillStyle, setStrokeStyle
    , beginPath, moveTo, lineTo, closePath, arc, arcAC
    , fill, stroke
    ) where

import Graphics.UI.Threepenny.Core

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


-- | Draw a filled rectangle (using the current fillStyle) at position (x, y) with width w and height h.
fillRect :: Int -> Int -> Int -> Int -> Canvas -> UI()
fillRect x y w h canvas =
  runFunction $ ffi "%1.getContext('2d').fillRect(%2, %3, %4, %5)" canvas x y w h

-- | Set the color or style to use inside shapes. Default #000 (black).
setFillStyle :: String -> Canvas -> UI()
setFillStyle sty canvas =
  runFunction $ ffi "%1.getContext('2d').fillStyle = %2" canvas sty

-- | Set the color or style to use for the lines around shapes. Default #000 (black).
setStrokeStyle :: String -> Canvas -> UI()
setStrokeStyle sty canvas =
  runFunction $ ffi "%1.getContext('2d').strokeStyle = %2" canvas sty



-- | Starts a new path by resetting the list of sub-paths. Call this function when you want to create a new path.
beginPath :: Canvas -> UI()
beginPath = runFunction . ffi "%1.getContext('2d').beginPath()"

-- | Moves the starting point of a new subpath to the (x, y) coordinates.
moveTo :: Vector -> Canvas -> UI()
moveTo (x,y) canvas =
  runFunction $ ffi "%1.getContext('2d').moveTo(%2, %3)" canvas x y

-- | Connects the last point in the subpath to the x, y coordinates with a straight line.
lineTo :: Vector -> Canvas -> UI()
lineTo (x,y) canvas =
  runFunction $ ffi "%1.getContext('2d').lineTo(%2, %3)" canvas x y

-- | Draw a straight line from the current point to the start. If the shape has already been closed or has only one point, this function does nothing.
closePath :: Canvas -> UI()
closePath = runFunction . ffi "%1.getContext('2d').closePath()"

-- | Adds an arc to the path which is centered at (x, y) position with
-- radius r starting at startAngle and ending at endAngle going in
-- clockwise direction.
arc :: Vector -> Int -> Int -> Int -> Canvas -> UI () -- TODO should really be using floats here
arc (x,y) radius startAngle endAngle canvas =
  runFunction $ ffi "%1.getContext('2d').arc(%2, %3, %4, %5, %6)" canvas x y radius startAngle endAngle

-- | Like 'arc' but in anticlockwise direction
arcAC :: Vector -> Int -> Int -> Int -> Canvas -> UI () -- TODO should really be using floats here
arcAC (x,y) radius startAngle endAngle canvas =
  runFunction $ ffi "%1.getContext('2d').arc(%2, %3, %4, %5, %6, true)" canvas x y radius startAngle endAngle


-- | Fills the subpaths with the current fill style.
fill :: Canvas -> UI ()
fill = runFunction . ffi "%1.getContext('2d').fill()"

-- | Strokes the subpaths with the current stroke style.
stroke :: Canvas -> UI ()
stroke = runFunction . ffi "%1.getContext('2d').stroke()"
