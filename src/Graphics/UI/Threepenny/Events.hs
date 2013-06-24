module Graphics.UI.Threepenny.Events (
    -- * Synopsis
    -- | Predefined DOM events.
    
    -- * Documentation
    click, hover, blur, leave,
    
    -- * Drag and Drop
    dragStart, dragEnter, dragOver, dragLeave, drag, drop, dragEnd,
    ) where

import Graphics.UI.Threepenny.Core
import Prelude hiding (drop)

silence = fmap (const ())

-- | Mouse click.
click :: Element -> Event ()
click = silence . domEvent "click"

-- | Mouse hovering over an element.
hover :: Element -> Event ()
hover = silence . domEvent "mouseenter"

-- | Mouse leaving an element.
leave :: Element -> Event ()
leave = silence . domEvent "mouseleave"

-- | Element loses focus.
blur :: Element -> Event ()
blur = silence . domEvent "blur"

-- | Data carried by a dragged element. 
--
-- FIXME: Empty data is currently encoded by the empty String.
-- Change this to 'Maybe String' instead.
type DragData = String

withDragData = fmap extract
    where
    extract (EventData [Just s]) = s
    extract _                    = ""

-- | Drag starts.
dragStart :: Element -> Event DragData
dragStart = withDragData . domEvent "dragstart"

-- | Drag enter.
dragEnter :: Element -> Event DragData
dragEnter = withDragData . domEvent "dragenter"

-- | Drag over event.
dragOver :: Element -> Event DragData
dragOver = withDragData . domEvent "dragover"

-- | Drag leave event.
dragLeave :: Element -> Event DragData
dragLeave = withDragData . domEvent "dragleave"

-- | Drag event.
drag :: Element -> Event DragData
drag = withDragData . domEvent "drag"

-- | Drop event.
drop :: Element -> Event DragData
drop = withDragData . domEvent "drop"

-- | Drag end event.
dragEnd :: Element -> Event DragData
dragEnd = withDragData . domEvent "dragend"
