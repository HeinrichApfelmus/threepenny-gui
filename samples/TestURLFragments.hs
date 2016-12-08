import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = startGUI defaultConfig $ \w -> do
    getBody w #+
        [ UI.button # set UI.id_ "me" # set UI.text "Hello"
        , UI.a # set UI.href "#me" # set UI.text "Click me"
        ]
    return ()