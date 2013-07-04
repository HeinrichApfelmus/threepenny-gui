module Graphics.UI.Threepenny.DragNDrop (
    -- * Synopsis
    -- | APi for handling drag and drop operations.
    -- 
    -- The drag and drop model implementation here is intended to be
    -- simpler than the HTML specification. See the documentation below for details.
    -- 
    -- Note: The implementation here is still a little preliminary.
    
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
--
-- An element with draggable set to 'True' will receive
-- 'drag', 'dragStart' and 'dragEnd' events.
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
--
-- An element with 'droppable' set to 'True' will receive
-- 'drop', 'dragOver', 'dragEnter' and 'dragLeave' events.
--
-- Child elements of a 'droppable' element may also be 'droppable'.
-- When dragging something over an element, the closest ancestor element
-- that is 'droppable' will be the target and receive corresponding
-- events.
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

-- | Occurs periodically while the element is being dragged around.
drag :: Element -> Event DragData
drag = withDragData . domEvent "drag"

-- | Dragging the element starts.
dragStart :: Element -> Event DragData
dragStart = withDragData . domEvent "dragstart"

-- | Dragging the element ends.
dragEnd :: Element -> Event DragData
dragEnd = withDragData . domEvent "dragend"

-- | The element is now the current target element for a 'drop'.
dragEnter :: Element -> Event DragData
dragEnter = withDragData . domEvent "dragenter"

-- | Occurs periodically while the element is the current target element.
dragOver :: Element -> Event DragData
dragOver = withDragData . domEvent "dragover"

-- | The element is no longer the current target element for a 'drop'.
dragLeave :: Element -> Event DragData
dragLeave = withDragData . domEvent "dragleave"

-- | The drag and drop operation is being completed on this element.
drop :: Element -> Event DragData
drop = withDragData . domEvent "drop"
