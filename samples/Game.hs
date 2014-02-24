{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Exception
import Control.Monad
import Data.Functor
import Data.Time
import Prelude hiding (catch,div,span)

import Paths

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Main application
------------------------------------------------------------------------------}

main :: IO ()
main = do
    static   <- getStaticDir
    messages <- Chan.newChan
    startGUI Config
        { tpCustomHTML = Nothing
        , tpStatic     = static
        } $ setup messages

type Message = (UTCTime, String, String)

setup :: Chan Message -> Window -> IO ()
setup globalMsgs w = do
    msgs <- Chan.dupChan globalMsgs

    return w # set title "Game"

    model <- Model <$> loadTiles "../wwwroot/game/map.txt" <*> return (0,0)
    body  <- getBody w
    (el, view) <- withWindow w $ mkView model
    addTo body [element el]

{-----------------------------------------------------------------------------
    Model
------------------------------------------------------------------------------}
type Position = (Int, Int)
data Tile  = Empty | Wall
type Tiles = [[Tile]]

data Model = Model
    { mTiles  :: Tiles
    , mPlayer :: Position 
    }

loadTiles :: FilePath -> IO Tiles
loadTiles filename = readTiles <$> readFile filename

readTiles :: String -> Tiles
readTiles = map (map toTile) . lines
    where
    toTile '.' = Empty
    toTile '#' = Wall

{-----------------------------------------------------------------------------
    View
------------------------------------------------------------------------------}
data View = View
    { vTiles  :: [[Element]]
    , vPlayer :: Element
    }

mkView :: Model -> Dom (Element, View)
mkView Model{..} = do
    vTiles  <- mapM (mapM mkTile) mTiles
    vPlayer <- div
        ! style [("position","absolute"), ("left","0px"), ("top", "0px")] $ []
    el      <- div ! style [("position","relative")] $ [grid (map (map element) vTiles)]
    return (el, View{..})

mkTile :: Tile -> Dom Element
mkTile tile = do
    el <- image ! width "32" ! height "32"
    liftIO $ updateTile tile el
    return el

updateTiles :: Tiles -> View -> IO ()
updateTiles tiles view = void . mapM sequence
    $ zipWith (zipWith updateTile) tiles (vTiles view)

updateTile :: Tile -> Element -> IO ()
updateTile Empty el = return el # set (attr "src") "static/game/floor.png" # void
updateTile Wall  el = return el # set (attr "src") "static/game/wall.png"  # void

