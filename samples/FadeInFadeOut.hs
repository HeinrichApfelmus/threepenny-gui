import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Graphics.UI.Threepenny.JQuery

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = do
    return w # set title "fadeIn - fadeOut"

    button <- UI.button # set text "Click me to make me fade out and in!"
    getBody w #+ [column [UI.string "Demonstration of jQuery's animate() function"
                         ,element button]]
    
    on UI.click button $ \_ -> do
        fadeOut button 400 Swing $ do
            runUI w $ fadeIn button 400 Swing $ return ()
