import Data.Array
import Control.Monad

import Paths
import System.FilePath

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = do
    static <- getStaticDir
    startGUI defaultConfig
        { tpPort       = 10000
        , tpStatic     = Just static
        } setup

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Game Thing"
    
    images <- mapM mkImage $ words "BlackMage floor wall"

    bCurrentImage <- stepper (head images) $
        head <$> unions [img <$ UI.click img | img <- images]

    canvas <- mkBoard window bCurrentImage
    
    getBody window #+
        [row (map element images)
        ,element canvas
        ]
    
    onChanges bCurrentImage $ \img -> do
        element img # set style [("border", "solid black 1px")]


{-----------------------------------------------------------------------------
    Board
------------------------------------------------------------------------------}
type Image = Maybe Element
type Board = Array (Int,Int) Image

mkBoard :: Window -> Behavior Element -> UI Element
mkBoard window bCurrentImage = do
    canvas <- UI.canvas
        # set UI.height 320
        # set UI.width  320
        # set style [("border", "solid black 1px")]

    bBoard <- accumB emptyBoard $
        place <$> bCurrentImage <@> UI.mousedown canvas

    onChanges bBoard $ \board -> do
        forM_ (assocs board) $ \(pos,mimg) -> case mimg of
            Just image -> UI.drawImage image (gridToPixels pos) canvas
            Nothing    -> return ()

    return canvas

width, height :: Int
width  = 10
height = 10

emptyBoard = array ((1,1),(width,height))
    [((x,y), Nothing) | x <- [1..width], y <- [1..height]]

place :: Element -> (Int,Int) -> (Board -> Board)
place image pixels board = board // [(location, Just image)]
    where location = pixelsToGrid pixels

pixelsToGrid (x,y) = (x `div` 32 + 1, y `div` 32 + 1)
gridToPixels (x,y) = (32*(x-1),32*(y-1))

{-----------------------------------------------------------------------------
    Images
------------------------------------------------------------------------------}
getImagePath :: String -> FilePath
getImagePath s = "static" </> "game" </> s <.> "png"

mkImage :: String -> UI Element
mkImage s = UI.img # set UI.src (getImagePath s)




