import Control.Monad
import Data.IORef

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- | Main entry point.
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = do
    return w # set title "Element Test"

    button1 <- UI.button # set UI.text "by tag"
    button2 <- UI.button # set UI.text "by class"
    button3 <- UI.button # set UI.text "by id 'me'" # set UI.id_ "me"
    button4 <- UI.button # set UI.text "by id 'ma'"

    let mkString s = UI.string s # set UI.class_ "string"

    getBody w #+ [column $
        row [element button1, element button2, element button3, element button4]
        : map mkString (words "Many different elements")]

    on UI.click button1 $ const $ do
        xs <- getElementsByTagName w "span"
        forM_ xs $ \x -> element x # set text "tag"

    on UI.click button2 $ const $ do
        xs <- getElementsByClassName w "string"
        forM_ xs $ \x -> element x # set text "class"

    Just button3 <- getElementById w "me"
    ref <- liftIO $ newIORef True
    on UI.click button3 $ const $ void $ do
        Just x <- getElementById w "me"
        b <- liftIO $ readIORef ref
        let s = if b then "yay" else "wow"
        element x # set UI.text s
        liftIO $ writeIORef ref (not b)

    on UI.click button4 $ const $ void $ do
        m <- getElementById w "ma"
        case m of
            Nothing -> element button4 # set UI.text "not found"
            Just el -> element el      # set UI.text "found"
