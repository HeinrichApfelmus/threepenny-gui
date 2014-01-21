-- | this example implements a simple mandelbrot-viewer demonstrating:
--
--   * using a Widget
--   * concurrent event-processing
--   * Repa for parallel processing (do not forget the +RTS -N flags)
--   * JuicyPixels for rendering PNGs
--
-- To compile/run the demo please make sure to install/configure using the buildExamples-Flag like this:
--
-- > cabal install --dependencies-only -fbuildExamples
-- > cabal configure -fbuildExamples
-- > cabal build
--
-- to run the example you have to know that it assumes some local folder-structure to find it's static paths
-- so please run it from the you main-folder (threepenny-gui) like this:
--
-- > ./dist/build/threepenny-examples-mandelbrot/threepenny-examples-mandelbrot
--
-- Alternativly you can just adjust the static-folder to your desired absolut path by chaning
-- the contentPath - constant in MandelbrotWidget.hs
--
-- The example consists of 3 source files - this beeing the main-entry point where the main-window is plugged together
-- Mandelbrot.hs handles implements a very basic version of the mandelbrot-rendering-algorithm
-- MandelbrotWidget.hs implements a threepenny-gui-widget, helpers and data-structures to render/display the fractal using threepenny-gui
--
module Main where

import MandelbrotWidget

import Control.Monad

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Elements as E
import qualified Graphics.UI.Threepenny.Attributes as A

main :: IO ()
main = do

    let settings = defaultSettings

    -- create the start-image if neccessary
    verifyStartImageFile settings

    putStrLn "starting local server..."
    startGUI defaultConfig
        { tpPort = 10000
        , tpStatic = Just contentPath
        } (setup settings)

defaultSettings :: Settings
defaultSettings = Settings defaultRes defaultZoom defaultSteps
    where
    defaultRes     = Size 1024 711
    defaultSteps   = 1024
    defaultZoom    = 4

setup :: Settings -> Window -> UI ()
setup settings w = void $ do
        E.addStyleSheet w "styles.css"
        return w # set title "Mandelbrot generator "

        md <- mandelbrotDisplay settings
        output <- E.div # set A.class_ "output" #+ [element md]

        statusText <- string promptClick #
           sink text (statusForRendering <$> isRendering md)

        mousePosText <- string "[-]" #
            sink text (statusForMouseCoords <$> mousePos md)

        status <- E.ul # set A.class_ "status" #+ [E.li #+ [element statusText], E.li #+ [element mousePosText]]

        getBody w #+ [element output, element status]

statusForRendering :: Bool -> String
statusForRendering True  = waitMessage
statusForRendering False = promptClick

statusForMouseCoords :: Compl -> String
statusForMouseCoords c = "[" ++ show c ++ "]"

promptClick :: String
promptClick = "click anywhere to zoom in to this point"

waitMessage :: String
waitMessage = "rendering please wait ..."

