import Control.Monad (void)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Enabled
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = void $ do
    return w # set title "Input Elements"
    
    button1 <- UI.button # set text "Me"
    button2 <- UI.button # set text "You"

    on UI.click button1 $ const $ do
        element button1 # set UI.enabled False
        element button2 # set UI.enabled True 
    on UI.click button2 $ const $ do
        element button2 # set UI.enabled False
        element button1 # set UI.enabled True 

    checkbox1 <- UI.input # set UI.type_ "checkbox"
    status    <- UI.span
    on UI.click checkbox1 $ const $ do
        b <- get UI.checked checkbox1
        element status # set text ("checked:" ++ show b)

    getBody w #+ [grid
        [[string "enabled", element button1, element button2]
        ,[string "checked", element checkbox1, element status]
        ]]
