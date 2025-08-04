import           Control.Monad                      (void)

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.SVG  as SVG

{-----------------------------------------------------------------------------
    SVG
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = void $ do
    return w # set title "SVG"

    heading <- UI.h1 # set text "SVG Two Ways"

    getBody w #+ [element heading
                 , UI.div #+ [svgElems, UI.h3 # set text "Individual SVG elements in Haskell"]
                 , UI.div # set html strCircle #+ [UI.h3 # set text "SVG block as a Haskell string"]
                 ]


svgElems :: UI Element
svgElems = do
    context <- SVG.svg
        # set SVG.width "150"
        # set SVG.height "100"
    elemCircle <- SVG.circle
        # set SVG.cx "100"
        # set SVG.cy "50"
        # set SVG.r "40"
        # set SVG.stroke "green"
        # set SVG.stroke_width "4"
        # set SVG.fill "yellow"
    return context #+ [element elemCircle]


strCircle :: String
strCircle = "<svg width=\"150\" height=\"100\">"
         ++ "  <circle cx=\"100\" cy=\"50\" r=\"40\" stroke=\"gray\" stroke-width=\"4\" fill=\"orange\" />"
         ++ "</svg>"

