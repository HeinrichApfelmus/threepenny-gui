module Graphics.UI.Threepenny.Canvas (
    -- * Synopsis
    -- | Partial binding to the HTML5 canvas API.
    
    -- * Documentation
    Canvas,
    Vector, drawImage, clearCanvas,
    ) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Internal.Core as Core
import qualified Graphics.UI.Threepenny.Internal.Types as Core

{-----------------------------------------------------------------------------
    Canvas
------------------------------------------------------------------------------}
type Canvas = Element

type Vector = (Int,Int)

-- | Draw the image of an image element onto the canvas at a specified position.
drawImage :: Element -> Vector -> Canvas -> IO ()
drawImage eimage (x,y) = updateElement $ \(Core.Element canvas window) -> do
    image <- manifestElement window eimage
    runFunction window $
        ffi "%1.getContext('2d').drawImage(%2,%3,%4)" canvas image x y

-- | Clear the canvas
clearCanvas :: Canvas -> IO ()
clearCanvas = updateElement $ \(Core.Element canvas window) -> do
    runFunction window $
        ffi "%1.getContext('2d').clear()" canvas


