{-----------------------------------------------------------------------------
    Threepenny

    HTML and CSS
------------------------------------------------------------------------------}

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

main = startGUI defaultConfig { jsStatic = Just "." } setup

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "HTML and CSS"
    UI.addStyleSheet window "foundation-5.css"
    
    left  <- UI.div
        #. "small-4 columns"
        #+  [ UI.h1     # set UI.text "Left"
            , UI.button # set UI.text "Click me"
            ]
    right <- UI.div
        #. "small-8 columns"
        #+  [ UI.h1  # set UI.text "Right"
            , UI.div #. "panel" #+
                [ UI.span # set UI.text "This and subsequent examples use "
                , UI.a
                    # set UI.href "https://foundation.zurb.com/sites/docs/v/5.5.3"
                    # set UI.text "Foundation"
                , UI.span # set UI.text ", a CSS framework."
                ]
            ]
    
    getBody window #+
        [ UI.div #. "row" #+
            [ element left
            , element right
            ]
        ]
    return ()
