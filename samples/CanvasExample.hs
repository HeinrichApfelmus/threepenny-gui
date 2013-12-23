import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig { tpPort = 10000 } setup

setup :: Window -> UI ()
setup window = do
    return window # set title "CanvasExample"

    canvas <- UI.canvas
        # set UI.height 400
        # set UI.width  400
        # set style [("border", "solid black 1px")]
    clear  <- UI.button #+ [string "Clear the canvas."]

    getBody window #+ [column
        [element canvas, string "Click to see some rectangles"]
        ,element clear
        ]

    let rects = [(75,100, 10, 10, "blue"), (325, 100, 15, 15, "blue"),
                 (200, 200, 25, 40, "orange"),
                 (250, 300, 75, 15, "plum")]

    let drawRect (x,y,w,h,col) = do
          UI.setFillStyle col canvas
          UI.fillRect x y w h canvas

    on UI.click canvas $ const $ forM_ rects drawRect

    on UI.click clear  $ const $ do
        UI.clearCanvas canvas
