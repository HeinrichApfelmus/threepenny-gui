module Graphics.UI.Threepenny.DragNDrop (
    -- * Synopsis
    -- | API for handling drag and drop operations.
    -- 
    -- See the documentation below for details on the drag and drop model.
    -- 
    -- WARNING: Events in this module may not behave as expected.
    -- The model is currently implemented in terms of HTML 5 drag and drop,
    -- but unfortunately,
    -- the HTML 5 specification for drag and drop is horrible and
    -- browser implementations are buggy.
    
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

withDragData = fmap (extract . unsafeFromJSON)
    where
    extract [s] = s
    extract _   = ""

-- | Occurs periodically while the element is being dragged around.
drag :: Element -> Event DragData
drag = withDragData . domEvent "drag"

-- | Dragging the element starts.
dragStart :: Element -> Event DragData
dragStart = withDragData . domEvent "dragstart"

-- | Dragging the element ends.
--
-- WARNING: This event can occur both before and after a corresponding 'drop' event.
dragEnd :: Element -> Event DragData
dragEnd = withDragData . domEvent "dragend"

-- | The element is now the current target element for a 'drop'.
-- 
-- WARNING: This element is buggy when moving the mouse over child elements.
dragEnter :: Element -> Event DragData
dragEnter = withDragData . domEvent "dragenter"

-- | Occurs periodically while the element is the current target element.
dragOver :: Element -> Event DragData
dragOver = withDragData . domEvent "dragover"

-- | The element is no longer the current target element for a 'drop'.
--
-- WARNING: This event is also fired when the mouse is moved over a child element.
dragLeave :: Element -> Event DragData
dragLeave = withDragData . domEvent "dragleave"

-- | The drag and drop operation is being completed on this element.
drop :: Element -> Event DragData
drop = withDragData . domEvent "drop"
