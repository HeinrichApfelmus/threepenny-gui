{-# LANGUAGE CPP, PackageImports #-}

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe

#ifdef CABAL
import qualified  "threepenny-gui" Graphics.UI.Threepenny as UI
import "threepenny-gui" Graphics.UI.Threepenny.Core
#else
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
#endif
import Paths

{-----------------------------------------------------------------------------
    Drag'N'Drop example
------------------------------------------------------------------------------}
main :: IO ()
main = do
    static <- getStaticDir
    startGUI defaultConfig
        { tpPort       = 10000
        , tpStatic     = Just static
        } setup

setup :: Window -> IO ()
setup w = void $ do
    return w # set title "Drag 'N' Drop Example"
    UI.addStyleSheet w "DragNDropExample.css"
    
    pairs <- sequence $
        zipWith (mkDragPair w) (words "red green blue") (map (150*) [0..2])
    getBody w #+ concat [[element i, element o] | (i,o) <- pairs]

type Color = String

mkDragPair :: Window -> Color -> Int -> IO (Element, Element)
mkDragPair w color position = do
    elDrag <- UI.new #. "box-drag"
        # set UI.style [("left", show position ++ "px"), ("color",color)]
        # set text "Drag me!"
        # set UI.draggable True
        # set UI.dragData color

    elDrop  <- UI.new #. "box-drop"
        # set UI.style [("border","2px solid " ++ color), ("left", show position ++ "px")]


    dropSuccess <- newIORef False

    on UI.dragStart elDrag $ \_ -> void $
        element elDrop
            # set text "Drop here!"
            # set UI.droppable True
    on UI.dragEnd   elDrag $ \_ -> void $ do
        dropped <- readIORef dropSuccess
        when (not dropped) $ void $
            element elDrop
                # set text ""
                # set UI.droppable False

    on UI.drop elDrop $ \color' -> when (color == color') $ void $ do
        writeIORef dropSuccess True
        delete elDrag
        element elDrop
            # set text "Dropped!"
            # set UI.droppable False
    
    return (elDrag, elDrop)

