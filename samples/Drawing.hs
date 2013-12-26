import Control.Monad

import Paths
import System.FilePath

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Drawing"
    
    canvas <- UI.canvas
        # set UI.height 400
        # set UI.width  400
        # set style [("border", "solid black 1px")]
    clear  <- UI.button #+ [string "Clear the canvas."]
    
    getBody window #+ [column
        [element canvas, string "Click to places images."]
        ,element clear
        ]
    
    dir <- liftIO $ getStaticDir
    url <- loadFile "image/png" (dir </> "game" </> "wizard-blue" <.> "png")
    img <- UI.img # set UI.src url
    
    let positions = [(x,y) | x <- [0,20..300], y <- [0,20..300]] :: [(Int,Int)]
    on UI.click canvas $ const $ forM_ positions $
        \xy -> UI.drawImage img xy canvas

    on UI.click clear  $ const $ do
        UI.clearCanvas canvas


