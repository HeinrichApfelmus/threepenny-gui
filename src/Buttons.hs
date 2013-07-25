{-# LANGUAGE CPP, PackageImports #-}


import Control.Monad
import Control.Concurrent (threadDelay)

#ifdef CABAL
import qualified "threepenny-gui" Graphics.UI.Threepenny as UI
import "threepenny-gui" Graphics.UI.Threepenny.Core
#else
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
#endif
import Paths

{-----------------------------------------------------------------------------
    Buttons
------------------------------------------------------------------------------}

main :: IO ()
main = do
    static <- getStaticDir
    startGUI Config
        { tpPort       = 10000
        , tpCustomHTML = Nothing
        , tpStatic     = static
        } setup

setup :: Window -> IO ()
setup w = void $ do
    return w # set title "Buttons"
    UI.addStyleSheet w "buttons.css"

    buttons <- mkButtons
    getBody w #+
        [UI.div #. "wrap" #+ (greet ++ map element buttons ++ [viewSource])]

greet :: [IO Element]
greet =
    [ UI.h1  #+ [string "Hello, Haskell!"]
    , UI.div #+ [string "Try the buttons below, they hover and click."]
    ]


mkButton :: String -> IO (Element, Element)
mkButton title = do
    button <- UI.button #. "button" #+ [string title]
    view   <- UI.p #+ [element button]
    return (button, view)

mkButtons :: IO [Element]
mkButtons = do
    list    <- UI.ul #. "buttons-list"
    
    (button1, view1) <- mkButton button1Title
    
    on UI.hover button1 $ \_ -> do
        element button1 # set text (button1Title ++ " [hover]")
    on UI.leave button1 $ \_ -> do
        element button1 # set text button1Title
    on UI.click button1 $ \_ -> do
        element button1 # set text (button1Title ++ " [pressed]")
        threadDelay $ 1000 * 1000 * 1
        element list    #+ [UI.li # set html "<b>Delayed</b> result!"]
    
    (button2, view2) <- mkButton button2Title

    on UI.hover button2 $ \_ -> do
        element button2 # set text (button2Title ++ " [hover]")
    on UI.leave button2 $ \_ -> do
        element button2 # set text button2Title
    on UI.click button2 $ \_ -> do
        element button2 # set text (button2Title ++ " [pressed]")
        element list    #+ [UI.li # set html "Zap! Quick result!"]
    
    return [list, view1, view2]

  where button1Title = "Click me, I delay a bit"
        button2Title = "Click me, I work immediately"

viewSource :: IO Element
viewSource = UI.p #+
    [UI.anchor #. "view-source" # set UI.href url #+ [string "View source code"]]
    where
    url = "https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/src/Buttons.hs"

