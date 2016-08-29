import Control.Monad

import Paths

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = do
    static <- getStaticDir
    startGUI defaultConfig { jsStatic = Just static } setup

canvasSize = 400

setup :: Window -> UI ()
setup window = do
    return window # set title "Canvas - Examples"

    canvas <- UI.canvas
        # set UI.height canvasSize
        # set UI.width  canvasSize
        # set style [("border", "solid black 1px"), ("background", "#eee")]

    drawRects <- UI.button #+ [string "Add some rectangles."]
    drawText  <- UI.button #+ [string "Add text."]
    drawImage <- UI.button #+ [string "Add image."]
    drawPie   <- UI.button #+ [string "Must have pie!"]
    clear     <- UI.button #+ [string "Clear the canvas."]

    getBody window #+
        [ column [element canvas]
        , element drawRects, element drawText, element drawImage
        , element drawPie  , element clear
        ]

    on UI.click clear $ const $
        canvas # UI.clearCanvas

    -- draw a pie chart
    on UI.click drawPie $ const $ do
        let
            center = (fromIntegral canvasSize / 2, fromIntegral (canvasSize+30) / 2)
            radius = 100

            drawSlice start end color = do
                canvas # set' UI.fillStyle (UI.htmlColor color)
                canvas # set' UI.strokeStyle "white"
                canvas # UI.beginPath
                canvas # UI.arc center radius start end
                canvas # UI.lineTo center
                canvas # UI.closePath
                canvas # UI.fill
                canvas # UI.stroke

            radian angle = angle * pi / 180

            normalizeAngles xs = map (\(x,y) -> (360 * x/total,y)) xs
                where total = sum $ map fst xs

            pieData = normalizeAngles
                [ (100, "#1f77b4")
                , (45, "#ff7f0e")
                , (80, "#2ca02c")
                , (10, "#d62728")
                , (105,"#9467bd")
                , (20, "#8c564b")
                ]

        UI.timestamp -- measure drawing performance for fun
        foldM (\start (delta, col) -> do
            let end = start+delta
            drawSlice (radian start) (radian end) col
            return end) 0 pieData
        UI.timestamp


    -- draw some rectangles
    on UI.click drawRects $ const $ do
        let rects = [ (20 , 130, 15, 120, "teal")
                    , (345, 110, 15, 90, "lightblue")
                    , (220, 360, 95, 15, "teal")
                    ]

        forM_ rects $ \(x,y,w,h,color) -> do
            canvas # set' UI.fillStyle (UI.htmlColor color)
            canvas # UI.fillRect (x,y) w h

    -- draw some text
    on UI.click drawText $ const $ do
        return canvas
            # set UI.textFont    "30px sans-serif"
            # set UI.strokeStyle "gray"
            # set UI.fillStyle   (UI.htmlColor "black")

        canvas # UI.strokeText "is awesome" (141,61)
        canvas # UI.fillText   "is awesome" (140,60)

    -- draw the haskell logo
    img <- UI.img # set UI.src "static/haskell-logo.png"

    on UI.click drawImage $ const $ do
        canvas # UI.drawImage img (60,20)

