{-----------------------------------------------------------------------------
    threepenny-gui
    
    Example:
    Small database with CRUD operations and filtering.
    To keep things simple, the list box is rebuild every time
    that the database is updated. This is perfectly fine for rapid prototyping.
    A more sophisticated approach would use incremental updates.
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}

import Prelude hiding (lookup)
import Control.Monad (void, when)
import Data.List (isPrefixOf)
import Data.Maybe
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (delete)

import Reactive.Threepenny (onChange)

import Tidings

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig { tpPort = 10000 } setup

setup :: Window -> UI ()
setup window = void $ mdo
    return window # set title "CRUD Example (Simple)"

    -- GUI layout
    listBox     <- UI.select # set (attr "size") "10" # set style [("width","200px")]
    createBtn   <- UI.button #+ [string "Create"]
    deleteBtn   <- UI.button #+ [string "Delete"]
    filterEntry <- UI.input
    firstname   <- UI.input
    lastname    <- UI.input
    
    let dataItem = grid [[string "First Name:", element firstname]
                        ,[string "Last Name:" , element lastname]]
    let glue = string " "
    getBody window #+ [grid
        [[row [string "Filter prefix:", element filterEntry], glue]
        ,[element listBox, dataItem]
        ,[row [element createBtn, element deleteBtn], glue]
        ]]

    let eCreate = UI.click createBtn
        eDelete = UI.click deleteBtn

    -- filter string
    tFilterString <- reactiveTextEntry filterEntry bFilterString
    bFilterString <- stepper "" $ rumors tFilterString
    let tFilter = isPrefixOf <$> tFilterString
        bFilter = facts  tFilter
        eFilter = rumors tFilter

    -- list box with selection
    eSelection <- rumors <$> reactiveListDisplay listBox
        bListBoxItems bSelection bShowDataItem
    -- data item display
    eDataItemIn <- rumors <$> reactiveDataItem (firstname,lastname)
        bSelectionDataItem

    -- database
    -- bDatabase :: Behavior (Database DataItem)
    let update' mkey x = flip update x <$> mkey
    bDatabase <- accumB emptydb $ concatenate <$> unions
        [ create ("Emil","Example") <$ eCreate
        , filterJust $ update' <$> bSelection <@> eDataItemIn
        , delete <$> filterJust (bSelection <@ eDelete)
        ]
                
    -- selection
    -- bSelection :: Behavior (Maybe DatabaseKey)
    bSelection <- stepper Nothing $ head <$> unions
        [ eSelection
        , Nothing <$ eDelete
        , Just . nextKey <$> bDatabase <@ eCreate
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
            <$> bSelection <*> bShowDataItem <@> eFilter
        ]
                
    let bLookup :: Behavior (DatabaseKey -> Maybe DataItem)
        bLookup = flip lookup <$> bDatabase
                
        bShowDataItem :: Behavior (DatabaseKey -> String)
        bShowDataItem = (maybe "" showDataItem .) <$> bLookup
                
        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems = (\p show -> filter (p. show) . keys)
                    <$> bFilter <*> bShowDataItem <*> bDatabase

        bSelectionDataItem :: Behavior (Maybe DataItem)
        bSelectionDataItem = (=<<) <$> bLookup <*> bSelection

    -- automatically enable / disable editing
    let
        bDisplayItem :: Behavior Bool
        bDisplayItem = maybe False (const True) <$> bSelection
    
    element deleteBtn # sink UI.enabled bDisplayItem
    element firstname # sink UI.enabled bDisplayItem
    element lastname  # sink UI.enabled bDisplayItem


{-----------------------------------------------------------------------------
    Database Model
------------------------------------------------------------------------------}
type DatabaseKey = Int
data Database a  = Database { nextKey :: !Int, db :: Map.Map DatabaseKey a }

emptydb = Database 0 Map.empty
keys    = Map.keys . db

create x     (Database newkey db) = Database (newkey+1) $ Map.insert newkey x db
update key x (Database newkey db) = Database newkey     $ Map.insert key    x db
delete key   (Database newkey db) = Database newkey     $ Map.delete key db
lookup key   (Database _      db) = Map.lookup key db

{-----------------------------------------------------------------------------
    Data items that are stored in the data base
------------------------------------------------------------------------------}
type DataItem = (String, String)
showDataItem (firstname, lastname) = lastname ++ ", " ++ firstname

-- single text entry
reactiveTextEntry
    :: Element
    -> Behavior String        -- text value
    -> UI (Tidings String)    -- user changes
reactiveTextEntry input bValue = do
    bEditing <- stepper False $ and <$>
        unions [True <$ UI.domEvent "focus" input, False <$ UI.blur input]
    
    window <- askWindow
    liftIOLater $ onChange bValue $ \s -> runUI window $ do
        editing <- liftIO $ currentValue bEditing
        when (not editing) $ void $ element input # set value s

    return $ tidings bValue (UI.valueChange input)

-- whole data item (consisting of two text entries)
reactiveDataItem
    :: (Element, Element)
    -> Behavior (Maybe DataItem)
    -> UI (Tidings DataItem)
reactiveDataItem (firstname,lastname) binput = do
    t1 <- reactiveTextEntry firstname (fst . maybe ("","") id <$> binput)
    t2 <- reactiveTextEntry lastname  (snd . maybe ("","") id <$> binput)
    return $ (,) <$> t1 <*> t2


{-----------------------------------------------------------------------------
    reactive list display
    
    Display a list of (distinct) items in a list box.
    The current selection contains one or no items.
    Changing the set may unselect the current item,
        but will not change it to another item.
------------------------------------------------------------------------------}
reactiveListDisplay :: forall a. Ord a
    => Element                  -- ListBox widget to use
    -> Behavior [a]             -- list of items
    -> Behavior (Maybe a)       -- selected element
    -> Behavior (a -> String)   -- display an item
    -> UI (Tidings (Maybe a))   -- current selection as item (possibly empty)
reactiveListDisplay w bitems bsel bdisplay = do
    -- animate output items
    element w # sink items (map <$> bdisplay <*> bitems)

    -- animate output selection
    let bindices :: Behavior (Map.Map a Int)
        bindices = (Map.fromList . flip zip [0..]) <$> bitems
        bindex   = lookupIndex <$> bindices <*> bsel

        lookupIndex indices Nothing    = Nothing
        lookupIndex indices (Just sel) = Map.lookup sel indices

    element w # sink UI.selection bindex

    -- changing the display won't change the current selection
    -- eDisplay <- changes display
    -- sink listBox [ selection :== stepper (-1) $ bSelection <@ eDisplay ]

    -- user selection
    let bindices2 :: Behavior (Map.Map Int a)
        bindices2 = Map.fromList . zip [0..] <$> bitems

    return $ tidings bsel $ lookupIndex <$> bindices2 <@> UI.selectionChange w


items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ map (\i -> UI.option # set text i) i

