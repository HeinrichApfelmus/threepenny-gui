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

setup :: Window -> UI ()
setup window = do
    return window # set title "Workaround for slow registering of event handlers"

    button <- UI.button
        # set UI.text "Click me"
        # set UI.id_  "button"
    msg    <- UI.span # set UI.text "Some text"
    
    getBody window #+ [ element button, element msg ]
    
    onElementId "button" "click" $ do
        element msg # set UI.text "I have been clicked!"

onElementId
    :: String   -- ID attribute of the element
    -> String   -- name of the DOM event to register the handler at
    -> UI void  -- handler to fire whenever the event happens
    -> UI ()
onElementId elid event handler = do
    window   <- askWindow
    exported <- ffiExport (runUI window handler >> return ())
    runFunction $ ffi "$(%1).on(%2,%3)" ("#"++elid) event exported
