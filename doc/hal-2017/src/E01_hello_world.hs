{-----------------------------------------------------------------------------
    Threepenny

    Hello world!
------------------------------------------------------------------------------}

-- imports
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core


-- start a Threepenny server that listens on port 8023 (this is the default)
main = startGUI (defaultConfig { jsPort = Just 8023 }) setup

-- build a user interface whenver a browser connects to the server
setup :: Window -> UI ()
setup window = do

    -- set window title
    return window # set UI.title "Hello World!"

    -- create a button element
    button <- UI.button # set UI.text "Click me!"
    -- attach button to the HTML body, so that it is displayed
    getBody window #+ [element button]
    
    -- register an event handler for clicking the button
    on UI.click button $ \_ -> do
        element button # set UI.text "I have been clicked!"
