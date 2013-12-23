module Graphics.UI.Threepenny.Canvas (
    -- * Synopsis
    -- | Partial binding to the HTML5 canvas API.

    -- * Documentation
    Canvas,
    Vector, drawImage, clearCanvas
    , fillRect, setFillStyle
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
