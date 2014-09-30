import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup


-- A data set that conveniently sum to 360,
-- and have labels that can be used as colors
dataset = [(100, "#1f77b4")
          , (45, "#ff7f0e")
          , (80, "#2ca02c")
          , (10, "#d62728")
          , (105,"#9467bd")
          , (20, "#8c564b")]

canvasSide = 416
center = (fromIntegral canvasSide / 2, fromIntegral canvasSide / 2)
radius = 100

radian angle = angle * pi / 180

setup :: Window -> UI ()
setup window = do
    return window # set title "Pie Chart"

    canvas <- UI.canvas
        # set UI.height canvasSide
        # set UI.width  canvasSide
        # set style [("border", "solid black 1px"), ("background", "lightgrey")]
        # set UI.lineWidth 2
        # set UI.strokeStyle "white"


    makePie <- UI.button #+ [string "Must have pie!"]
    clear   <- UI.button #+ [string "Clear the canvas."]

    getBody window #+ [column
        [element canvas]
        ,element makePie, element clear
        ]

    let drawSlice start end col = do
          canvas # set' UI.fillStyle (UI.htmlColor col)
          canvas # UI.beginPath
          canvas # UI.arc center radius start end
          canvas # UI.lineTo center
          canvas # UI.closePath
          canvas # UI.fill
          canvas # UI.stroke

    on UI.click makePie $ const $
      foldM (\start (delta, col) -> do
                let end = start+delta
                drawSlice (radian start) (radian end) col
                return end) 0 dataset

    on UI.click clear $ const $
        UI.clearCanvas canvas
