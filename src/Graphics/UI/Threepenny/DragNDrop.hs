module Graphics.UI.Threepenny.DragNDrop (
    -- * Synopsis
    -- | Attributes and events related to drag and drop.
    
    -- * Documentation
    draggable, droppable, dragData,
    DragData,
    drag, dragStart, dragEnd, drop, dragEnter, dragLeave, dragOver,
    ) where

import Prelude hiding (drop)
import Control.Monad
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Attributes
------------------------------------------------------------------------------}
-- | Enable or disable whether the element can be dragged by the user.
draggable :: WriteAttr Element Bool
draggable = mkWriteAttr set
    where
    set v = set' (attr "draggable") $ if v then "true" else "false"

-- | Set the data that is transferred when dragging this element.
dragData :: WriteAttr Element DragData
dragData = mkWriteAttr set
    where
    set v = set' (attr "ondragstart") $
        "event.dataTransfer.setData('dragData', '" ++ v ++ "')"

-- | Enable or disable whether the element accepts drops.
droppable :: WriteAttr Element Bool
droppable = mkWriteAttr enable
    where
    enable v = void . if v then allowDrop else blockDrop
    allowDrop el =
        element el
            # set (attr "ondragover") "event.preventDefault()"
            # set (attr "ondrop"    ) "event.preventDefault()"
    blockDrop el =
        element el
            # set (attr "ondragover") ""
            # set (attr "ondrop"    ) ""

{-----------------------------------------------------------------------------
    Events
------------------------------------------------------------------------------}
-- | Data carried by a dragged element. 
--
-- FIXME: Empty data is currently encoded by the empty String.
-- Change this to 'Maybe String' instead.
type DragData = String


withDragData = fmap extract
    where
    extract (EventData [Just s]) = s
    extract _                    = ""

-- | Drag event.
drag :: Element -> Event DragData
drag = withDragData . domEvent "drag"

-- | Dragging the element starts.
dragStart :: Element -> Event DragData
dragStart = withDragData . domEvent "dragstart"

-- | Dragging the element ends.
dragEnd :: Element -> Event DragData
dragEnd = withDragData . domEvent "dragend"

-- | Drag enter.
dragEnter :: Element -> Event DragData
dragEnter = withDragData . domEvent "dragenter"

-- | Drag over event.
dragOver :: Element -> Event DragData
dragOver = withDragData . domEvent "dragover"

-- | Drag leave event.
dragLeave :: Element -> Event DragData
dragLeave = withDragData . domEvent "dragleave"

-- | Something is being dropped on the element.
drop :: Element -> Event DragData
drop = withDragData . domEvent "drop"