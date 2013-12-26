{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as Map

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Paths
import VideoAnnotate.MediaPlayer

type Map = Map.Map

{-----------------------------------------------------------------------------
    UI
------------------------------------------------------------------------------}
main :: IO ()
main = do
    static <- getStaticDir
    startGUI defaultConfig { tpStatic = Just static } setup

setup :: Window -> IO ()
setup w = do
    return w # set title "Video Annotations"
    
    body <- getBody w
    withWindow w $ do
        filename <- UI.input
        load     <- UI.button # set text "Load"
        video    <- mkMediaPlayer
        
        element body #+ [column
            [   row [element filename, element load]
            ,   view video
            ]]
        
        liftIO $ on UI.click load $ \_ -> do
            path <- get value filename
            uri  <- loadFile w "video/mp4" path
            return video # set source uri

{-----------------------------------------------------------------------------
    Logic
------------------------------------------------------------------------------}
type Annotations = Map Time String



