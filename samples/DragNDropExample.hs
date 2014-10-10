import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe

import Paths

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Drag'N'Drop example
------------------------------------------------------------------------------}
main :: IO ()
main = do
    static <- getStaticDir
    startGUI defaultConfig { jsStatic = Just static } setup

setup :: Window -> UI ()
setup w = void $ do
    return w # set title "Drag 'N' Drop Example"
    UI.addStyleSheet w "DragNDropExample.css"
    
    pairs <- sequence $
        zipWith mkDragPair (words "red green blue") (map (150*) [0..2])
    getBody w #+ concat [[element i, element o] | (i,o) <- pairs]

type Color = String

mkDragPair :: Color -> Int -> UI (Element, Element)
mkDragPair color position = do
    elDrag <- UI.new #. "box-drag"
        # set UI.style [("left", show position ++ "px"), ("color",color)]
        # set text "Drag me!"
        # set UI.draggable True
        # set UI.dragData color

    elDrop  <- UI.new #. "box-drop"
        # set UI.style [("border","2px solid " ++ color), ("left", show position ++ "px")]


    dropSuccess <- liftIO $ newIORef False

    on UI.dragStart elDrag $ \_ -> void $
        element elDrop
            # set text "Drop here!"
            # set UI.droppable True
    on UI.dragEnd   elDrag $ \_ -> void $ do
        dropped <- liftIO $ readIORef dropSuccess
        when (not dropped) $ void $
            element elDrop
                # set text ""
                # set UI.droppable False

    on UI.drop elDrop $ \color' -> when (color == color') $ void $ do
        liftIO $ writeIORef dropSuccess True
        delete elDrag
        element elDrop
            # set text "Dropped!"
            # set UI.droppable False
    
    return (elDrag, elDrop)

