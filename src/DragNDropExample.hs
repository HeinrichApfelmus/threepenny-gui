{-# LANGUAGE CPP, PackageImports #-}

import Control.Applicative
import Control.Monad
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
    startGUI Config
        { tpPort       = 10000
        , tpCustomHTML = Nothing
        , tpStatic     = static
        } setup

setup :: Window -> IO ()
setup w = void $ do
    return w # set title "Drag 'N' Drop"
    
    pairs <- mapM (mkDragPair w) $ words "red green blue"
    getBody w #+ [grid [[element i, element o] | (i,o) <- pairs]]


type Color = String

mkDragPair :: Window -> Color -> IO (Element, Element)
mkDragPair w c = (,) <$> mkOut c <*> mkIn w c

mkOut :: Color -> IO Element
mkOut color = do
    UI.new
        # set UI.id_ (color ++ "_out")
        # set UI.style [("color",color)]
        # set text "Drag me!"
        # set UI.draggable True
        # set UI.dragData color

mkIn  :: Window -> Color -> IO Element
mkIn w color = do
    el <- UI.new
        # set UI.id_ (color ++ "_in")
        # set UI.style [("border","2px solid " ++ color)]
        # set text "Drop here!"
        # set UI.droppable True

    on UI.dragEnter el $ \color' -> when (color == color') $ void $
        element el
            # set UI.style [("color",color)]
    on UI.dragLeave el $ \_ ->
        element el
            # set UI.style [("color","black")]
    on UI.drop el $ \color' -> when (color == color') $ void $
        element el #+ [fromJust <$> getElementById w (color ++ "_out")]
        
    return el

