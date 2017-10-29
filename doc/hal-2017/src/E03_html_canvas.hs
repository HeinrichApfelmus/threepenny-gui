{-----------------------------------------------------------------------------
    Threepenny

    HTML Canvas
------------------------------------------------------------------------------}
module E03_html_canvas where

import Control.Monad (forM_)

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "HTML Canvas"
    getBody window #+ [ mkDisplay ]
    return ()

canvasX = 400
canvasY = 300

-- | Create canvas element and paint on it.
mkDisplay :: UI Element
mkDisplay = do
    canvas <- UI.canvas
        # set UI.width  canvasX
        # set UI.height canvasY
        # set style [("border", "solid black 1px"), ("background", "white")]

    let xs = [1,2,3,5,6]
        ys = [3,4,2,1,8]
    canvas # plotList xs ys
    return canvas

-- Very simple matlab style list plot 
plotList :: [Double] -> [Double] -> Element -> UI ()
plotList xs ys c = axes >> graph
    where
    sx = fromIntegral canvasX
    sy = fromIntegral canvasY
    lx = maximum xs - minimum xs
    ly = maximum ys - minimum ys
    d  = 20 :: Double
    screenX x = 0.9 * (sx - 2*d) * ((x-minimum xs)/lx) + 1.1 * d
    screenY y = sy - (0.9 * (sy - 2*d) * ((y-minimum ys)/ly) + 1.1 * d)
    
    axes = do
        c # set' UI.fillStyle (UI.htmlColor "black")
        -- x axis with arrowhead
        c # line (d,sy-d) (sx-d,sy-d)
        c # line (sx-1.5*d,sy-d-0.3*d) (sx-d,sy-d)
        c # line (sx-1.5*d,sy-d+0.3*d) (sx-d,sy-d)
        
        -- y axis
        c # line (d,sy-d) (d,d)
        c # line (d-0.3*d,d+0.5*d) (d,d)
        c # line (d+0.3*d,d+0.5*d) (d,d)

    graph = do
        c # set' UI.fillStyle (UI.htmlColor "darkblue")
        forM_ (zip xs ys) $ \(x,y) -> c # circle (screenX x, screenY y) 4

-- draw a single line
line :: UI.Point -> UI.Point -> Element -> UI ()
line xy1 xy2 c = do
    c # UI.beginPath
    c # UI.moveTo xy1
    c # UI.lineTo xy2
    c # UI.closePath
    c # UI.stroke

-- fill a circle with a given center and radius
circle :: UI.Point -> Double -> Element -> UI ()
circle xy radius c = do
    c # UI.beginPath
    c # UI.arc xy radius 0 (2*pi)
    c # UI.closePath
    c # UI.fill
