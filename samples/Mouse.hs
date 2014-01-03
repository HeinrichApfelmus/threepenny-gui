import Control.Monad (void)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = do
    return w # set title "Mouse"
    
    out  <- UI.span # set text "Coordinates: "
    wrap <- UI.div #. "wrap"
        # set style [("width","300px"),("height","300px"),("border","solid black 1px")]
        # set (attr "tabindex") "1" -- allow key presses
        #+ [element out]
    getBody w #+ [element wrap]
    
    on UI.mousemove wrap $ \xy ->
        element out # set text ("Coordinates: " ++ show xy)
    on UI.keydown   wrap $ \c ->
        element out # set text ("Keycode: " ++ show c)
