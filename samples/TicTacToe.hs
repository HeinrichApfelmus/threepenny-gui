{-----------------------------------------------------------------------------
    threepenny-gui

    Example:
    Tic Tac Toe using buttons for the squares.
    Based on the reactive-banana version,
    see https://www.haskell.org/haskellwiki/Reactive-banana/Examples.
------------------------------------------------------------------------------}
import           Paths

import           Control.Monad
import           Data.Array
import           Data.List.Split (chunksOf)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

main :: IO ()
main = do
  static <- getStaticDir
  startGUI defaultConfig { jsStatic = Just static } setup

setup :: Window -> UI ()
setup w = void $ do
  return w # set title "XOX" 
  UI.addStyleSheet w "tictactoe.css"

  -- GUI elements
  cells <- replicateM 9 $ UI.button

  let uiCells :: [UI Element]
      uiCells = map element cells

      events :: [Event ()]
      events = map UI.click cells

      moves :: Event (Game -> Game)
      moves = fmap concatenate . unions $ zipWith (\e s -> move s <$ e)
              events [(x,y) | y <- [1..3], x <- [1..3]]

  tictactoe <- UI.h1 #+ [string "Tic Tac Toe"]
  turn      <- UI.h2
  victory   <- UI.h2 # set style [("color", "crimson")]
  
  -- GUI layout
  getBody w #+ [ column
                 [ element tictactoe
                 , element turn
                 , grid (chunksOf 3 uiCells)
                 , element victory
                 ]
               ]
  
  -- events and behaviors
  eState <- accumE newGame moves
  bState <- stepper newGame eState
  bEnd   <- stepper Nothing (isGameEnd . board <$> eState)

  let bPlayer  = (show . player) <$> bState
      bReveal  = const [("color", "navy")] <$> bState 
      bDisable = const False <$> bState
      bWinner  = maybe "" (\x -> show x ++ " Wins!") <$> bEnd

  tokens   <- mapM (\e -> stepper "X"  (bPlayer  <@ e)) events
  revealed <- mapM (\e -> stepper []   (bReveal  <@ e)) events
  disabled <- mapM (\e -> stepper True (bDisable <@ e)) events

  zipWithM_ (\b e -> sink UI.text b e)    tokens   uiCells
  zipWithM_ (\b e -> sink UI.style b e)   revealed uiCells
  zipWithM_ (\b e -> sink UI.enabled b e) disabled uiCells 
  
  sink UI.text ((++ " to move") <$> bPlayer) $ element turn
  sink UI.text bWinner $ element victory

  onEvent (isGameEnd . board <$> eState) $ \t -> do
    case  t of
      Just _  -> mapM_ (set UI.enabled False) uiCells
      Nothing -> return ()

-----------------------------------------------------------------------------
-- | Game Logic
-----------------------------------------------------------------------------
  
data Token = Nobody | X | O
    deriving (Show, Eq)

-- |The coordinates of a square.
type Square = (Int,Int)

-- |A noughts and crosses board.
type Board = Array Square Token

-- |Returns an empty 'Board'.
newBoard :: Board
newBoard = listArray ((1,1),(3,3)) (repeat Nobody)

-- |Puts a 'Token' in a 'Square'.
setSquare :: Board -> Square -> Token -> Board
setSquare brd square token =
    if (brd ! square) /= Nobody
    then error $ "square " ++ show square ++ " is not empty"
    else brd // [(square, token)]

-- | Determine if the 'Board' is in an end state.
--   Returns 'Just' 'Token' if the game has been won,
--   'Just' 'Nobody' for a draw, otherwise 'Nothing'.
isGameEnd :: Board -> Maybe Token
isGameEnd brd
    | Just X `elem` maybeWins = Just X
    | Just O `elem` maybeWins = Just O
    | Nobody `notElem` elems brd = Just Nobody
    | otherwise = Nothing

    where rows :: [[Square]]
          rows = let i = [1..3]
                 in [[(x,y) | y <- i] | x <- i] ++ -- rows
                    [[(y,x) | y <- i] | x <- i] ++ -- coloumns
                    [[(x,x) | x <- i], [(x,4-x) | x <- i]] -- diagonals

          rows2tokens :: [[Token]]
          rows2tokens = map (map (brd !)) rows

          isWin :: [Token] -> Maybe Token
          isWin tokens
              | all (==X) tokens = Just X
              | all (==O) tokens = Just O
              | otherwise = Nothing

          maybeWins :: [Maybe Token]
          maybeWins = map isWin rows2tokens

-- | The state of a game, i.e. the player who's turn it is, and the current board.
data Game = Game { player :: Token, board :: Board }

newGame :: Game
newGame = Game X newBoard

-- | Puts the player's token on the specified square.
--   Returns 'Just' 'Token' if the game has been won,
--   'Just' 'Nobody' for a draw, otherwise 'Nothing'.
move :: Square -> Game -> Game
move square (Game plyr brd) = Game player' board'
    where
    board'  = setSquare brd square plyr
    player' = case plyr of {X -> O; O -> X; Nobody -> Nobody}
