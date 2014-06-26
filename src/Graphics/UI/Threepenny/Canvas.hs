module Graphics.UI.Threepenny.Canvas (
    -- * Synopsis
    -- | Partial binding to the HTML5 canvas API.
    
    -- * Documentation
    Canvas,
    Vector, fillStyle, fillRect, strokeStyle, strokeRect, drawImage, clearCanvas,
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

-- | Clear the canvas
clearCanvas :: Canvas -> UI ()
clearCanvas = runFunction . ffi "%1.getContext('2d').clear()"


