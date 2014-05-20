{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Graphics.UI.Threepenny.Widgets (
    -- * Synopsis
    -- | Widgets are reusable building blocks for a graphical user interface.
    -- This module collects useful widgets that are designed to work
    -- with functional reactive programming (FRP).
    -- 
    -- For more details and information on how to write your own widgets, see the
    -- <https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/doc/design-widgets.md widget design guide>.
    
    -- * Tidings
    Tidings, rumors, facts, tidings,
    -- * Widgets
    -- ** Input widgets
    TextEntry, entry, userText,
    -- ** ListBox
    ListBox, listBox, userSelection,
    ) where

import Control.Monad (void, when)
import qualified Data.Map                          as Map

import qualified Graphics.UI.Threepenny.Attributes as UI
import qualified Graphics.UI.Threepenny.Events     as UI
import qualified Graphics.UI.Threepenny.Elements   as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

{-----------------------------------------------------------------------------
    Input widgets
------------------------------------------------------------------------------}
-- | A single-line text entry.
data TextEntry = TextEntry
    { _elementTE :: Element
    , _userTE    :: Tidings String
    }

instance Widget TextEntry where getElement = _elementTE

-- | User changes to the text value.
userText :: TextEntry -> Tidings String
userText = _userTE

-- | Create a single-line text entry.
entry
    :: Behavior String  -- ^ Display value when the element does not have focus.
    -> UI TextEntry
entry bValue = do -- single text entry
    input <- UI.input

    bEditing <- stepper False $ and <$>
        unions [True <$ UI.focus input, False <$ UI.blur input]
    
    window <- askWindow
    liftIOLater $ onChange bValue $ \s -> runUI window $ do
        editing <- liftIO $ currentValue bEditing
        when (not editing) $ void $ element input # set value s

    let _elementTE = input
        _userTE    = tidings bValue $ UI.valueChange input 
    return TextEntry {..}

{-----------------------------------------------------------------------------
    List box
------------------------------------------------------------------------------}
-- | A list of values. The user can select entries.
data ListBox a = ListBox
    { _elementLB   :: Element
    , _selectionLB :: Tidings (Maybe a)
    }

instance Widget (ListBox a) where getElement = _elementLB

-- | User changes to the current selection (possibly empty).
userSelection :: ListBox a -> Tidings (Maybe a)
userSelection = _selectionLB

-- | Create a 'ListBox'.
listBox :: forall a. Ord a
    => Behavior [a]               -- ^ list of items
    -> Behavior (Maybe a)         -- ^ selected item
    -> Behavior (a -> UI Element) -- ^ display for an item
    -> UI (ListBox a)
listBox bitems bsel bdisplay = do
    list <- UI.select

    -- animate output items
    element list # sink items (map <$> bdisplay <*> bitems)

    -- animate output selection
    let bindices :: Behavior (Map.Map a Int)
        bindices = (Map.fromList . flip zip [0..]) <$> bitems
        bindex   = lookupIndex <$> bindices <*> bsel

        lookupIndex indices Nothing    = Nothing
        lookupIndex indices (Just sel) = Map.lookup sel indices

    element list # sink UI.selection bindex

    -- changing the display won't change the current selection
    -- eDisplay <- changes display
    -- sink listBox [ selection :== stepper (-1) $ bSelection <@ eDisplay ]

    -- user selection
    let bindices2 :: Behavior (Map.Map Int a)
        bindices2 = Map.fromList . zip [0..] <$> bitems

        _selectionLB = tidings bsel $
            lookupIndex <$> bindices2 <@> UI.selectionChange list
        _elementLB   = list

    return ListBox {..}

items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ map (\i -> UI.option #+ [i]) i




