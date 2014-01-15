import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set title "CanvasExample"

    canvas <- UI.canvas
        # set UI.height 400
        # set UI.width  400
        # set style [("border", "solid black 1px")]

    addRects <- UI.button #+ [string "Add some rectangles."]
    addArcs <- UI.button #+ [string "Add some arcs and circles."]
    clear  <- UI.button #+ [string "Clear the canvas."]

    getBody window #+ [column
        [element canvas]
        ,element addRects, element addArcs, element clear
        ]

    let rects = [(75,100, 10, 10, "blue"), (325, 100, 15, 15, "blue"),
                 (250, 300, 75, 15, "plum")]
    let drawRect (x,y,w,h,col) = do
          element canvas # set UI.fillStyle col
          UI.fillRect x y w h canvas

    on UI.click addRects $ const $ forM_ rects drawRect

    let circles = [ (200, 200, 25, "orange")
                  , (300, 180, 15, "plum")
                  , (100, 180, 15, "plum") ]
    let drawCircle (x,y,r,col) = do
          element canvas # set UI.fillStyle col
          UI.beginPath canvas
          UI.arc (x,y) r 0 (2*pi) canvas
          UI.fill canvas

    let slices = [ (325, 115, 25, 1, 2, "lightblue"), (325, 145, 25, 1, 2, "lightblue") ]
    let drawSlice (x,y,r,start,end,col) = do
          element canvas # set UI.fillStyle col
          UI.beginPath canvas
          UI.arc (x,y) r start end canvas
          UI.lineTo (x,y) canvas
          UI.stroke canvas
          UI.closePath canvas
          UI.fill canvas

    on UI.click addArcs $ const $ do
      forM_ circles drawCircle
      forM_ slices drawSlice
      element canvas # set UI.textFont "42pt sans-serif"
      UI.fillText "Canvas" (100,100) canvas
      UI.strokeText "Canvas" (100,100) canvas

    on UI.click clear  $ const $ UI.clearCanvas canvas
