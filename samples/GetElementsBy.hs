import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- | Main entry point.
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = do
    return w # set title "Element Test"

    button1 <- UI.button # set UI.text "tag"
    button2 <- UI.button # set UI.text "class"
    button3 <- UI.button # set UI.text "id"    # set UI.id_ "me"

    let mkString s = UI.string s # set UI.class_ "string"

    getBody w #+ [column $
        row [element button1, element button2, element button3]
        : map mkString (words "We are so different")]

    on UI.click button1 $ const $ do
        xs <- getElementsByTagName w "span"
        forM_ xs $ \x -> element x # set text "tag"

    on UI.click button2 $ const $ do
        xs <- getElementsByClassName w "string"
        forM_ xs $ \x -> element x # set text "class"

    on UI.click button3 $ const $ void $ do
        Just x <- getElementById w "me"
        element x # set UI.text "got me"

