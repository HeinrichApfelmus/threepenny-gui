import Data.Array
import Control.Monad

import Paths
import System.FilePath
import System.Directory

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = do
    static     <- getStaticDir
    playground <- mkPlayground
    startGUI defaultConfig
        { tpStatic     = Just static
        } $ setup playground

setup :: Playground -> Window -> UI ()
setup playground window = void $ do
    return window # set title "Game Thing"
    
    (images, eLoaded) <- loadImages =<< liftIO getImageUrls

    bCurrentImage <- stepper (head images) $
        head <$> unions [img <$ UI.click img | img <- images]

    canvas <- mkBoardView eLoaded playground bCurrentImage
    
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

type Move  = Board -> Board

type Playground = (Event Board, Behavior Board, Move -> IO ())

mkPlayground :: IO Playground
mkPlayground = do
    (e, fire) <- newEvent
    eBoard    <- accumE emptyBoard e
    bBoard    <- stepper emptyBoard eBoard
    return (eBoard, bBoard, fire)

onBehavior :: Behavior a -> (a -> UI ()) -> UI ()
onBehavior b f = do
    onChanges b f
    window <- askWindow
    liftIOLater $ runUI window $ f =<< currentValue b


-- | Create a canvas that reflects the current state of the board.
mkBoardView :: Event () -> Playground -> Behavior Element -> UI Element
mkBoardView eImagesLoaded (eBoard, bBoard,fireMove) bCurrentImage = do
    canvas <- UI.canvas
        # set UI.height 320
        # set UI.width  320
        # set style [("border", "solid black 1px")]

    onEvent (place <$> bCurrentImage <@> UI.mousedown canvas) $
        liftIO . fireMove

    let eDraw = unionWith const eBoard (bBoard <@ eImagesLoaded)
    onEvent eDraw $ \board -> do
        forM_ (assocs board) $ \(pos,mimg) -> case mimg of
            Just image -> UI.drawImage image (gridToPixels pos) canvas
            Nothing    -> return ()

    return canvas

width, height :: Int
width  = 10
height = 10

emptyBoard = array ((1,1),(width,height))
    [((x,y), Nothing) | x <- [1..width], y <- [1..height]]

place :: Element -> (Int,Int) -> Move
place image pixels board = board // [(location, Just image)]
    where location = pixelsToGrid pixels

pixelsToGrid (x,y) = (x `div` 32 + 1, y `div` 32 + 1)
gridToPixels (x,y) = (32*(x-1),32*(y-1))

{-----------------------------------------------------------------------------
    Images
------------------------------------------------------------------------------}
-- | Load image URLs from the resource directory
getImageUrls :: IO [String]
getImageUrls = do
    dir <- getStaticDir
    xs  <- getDirectoryContents (dir </> "game")
    return ["static/game/" ++ takeBaseName x ++ ".png"
           | x <- xs, takeExtension x == ".png"]

-- | Load image elements and fire an event when the images are all loaded.
loadImages :: [String] -> UI ([Element], Event ())
loadImages urls = do
    images <- mapM (\url -> UI.img # set UI.src url) urls
    e      <- accumE 0 $ (+1) <$ (unions $ map (domEvent "load") images)
    return (images, () <$ filterE (== length images) e)


