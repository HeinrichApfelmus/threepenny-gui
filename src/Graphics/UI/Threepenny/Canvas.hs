module Graphics.UI.Threepenny.Canvas (
    -- * Synopsis
    -- | Partial binding to the HTML5 canvas API.
    
    -- * Documentation
    Canvas,
    Vector, fillStyle, fillRect, strokeStyle, strokeRect, drawImage, clearRect, clearCanvas,
    -- | Create path
    beginPath, moveTo, lineTo, stroke,
    ) where

import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Canvas
------------------------------------------------------------------------------}
type Canvas = Element

type Vector = (Int,Int)

-- | Set the fill style
--
-- Example of usage
--
-- > fillStyle "#CAB" canvas
fillStyle :: String -> Canvas -> UI ()
fillStyle style canvas =
    runFunction $ ffi "%1.getContext('2d').fillStyle = %2" canvas style

-- | Draw a rectangle filled with the current fill style
fillRect :: Vector -> Vector -> Canvas -> UI ()
fillRect (x1, y1) (x2, y2) canvas =
    runFunction $ ffi "%1.getContext('2d').fillRect(%2,%3,%4,%5)" canvas x1 y1 x2 y2

-- | Set the stroke style
--
-- Example of usage
--
-- > strokeStyle "#CAB" canvas
strokeStyle :: String -> Canvas -> UI ()
strokeStyle style canvas =
    runFunction $ ffi "%1.getContext('2d').strokeStyle = %2" canvas style

-- | Drow a rectangle with the current strokeStyle
strokeRect :: Vector -> Int -> Int -> Canvas -> UI ()
strokeRect (x, y) width height canvas =
    runFunction $ ffi "%1.getContext('2d').strokeRect(%2,%3,%4,%5)" canvas x y width height

-- | Draw the image of an image element onto the canvas at a specified position.
drawImage :: Element -> Vector -> Canvas -> UI ()
drawImage image (x,y) canvas =
    runFunction $ ffi "%1.getContext('2d').drawImage(%2,%3,%4)" canvas image x y

-- | Clear the canvas in the specified rectangle
clearRect :: Vector -> Int -> Int -> Canvas -> UI ()
clearRect (x,y) width height canvas =
    runFunction $ ffi "%1.getContext('2d').clearRect(%2,%3,%4,%5)" canvas x y width height

-- | Clear the canvas
clearCanvas :: Canvas -> UI ()
clearCanvas = runFunction . ffi "%1.getContext('2d').clear()"

{-----------------------------------------------------------------------------
    Canvas Path
    
    This  function are primitive they should be wrap arround a Monad
-----------------------------------------------------------------------------}

{- |
 Drawing path primitive
 -}
beginPath :: Canvas -> UI ()
beginPath canvas =
    runFunction $ ffi "%1.getContext('2d').beginPath()" canvas

moveTo :: Vector -> Canvas -> UI ()
moveTo (x,y) canvas =
    runFunction $ ffi "%1.getContext('2d').moveTo(%2,%3)" canvas x y

lineTo :: Vector -> Canvas -> UI ()
lineTo (x,y) canvas =
    runFunction $ ffi "%1.getContext('2d').lineTo(%2,%3)" canvas x y

stroke :: Canvas -> UI ()
stroke canvas =
    runFunction $ ffi "%1.getContext('2d').stroke()" canvas
