
-- | Utility methods for handling particular events.

module Graphics.UI.Threepenny.Events (
    on,
    click, hover, blur,
    allowDrag, blockDrag, setDraggable, setDragData,
    allowDrop, blockDrop, setDroppable,
    dragStart, dragEnter, dragOver, dragLeave, drag, drop, dragEnd,
    ) where

import Graphics.UI.Threepenny
import Prelude hiding (drop)

-- | Convenience function to register 'Event's for 'Element's.
--
-- Example:
--
-- > on click element $ \_ -> ...
on :: (a -> Event b) -> a -> Handler b -> IO ()
on f a h = register (f a) h >> return ()

silence = fmap (const ())

-- | Mouse click.
click :: Element -> Event ()
click = silence . bind "click"

-- | Mouse hovering over an element.
hover :: Element -> Event ()
hover = silence . bind "mouseenter"

-- | Mouse leaving an element.
leave :: Element -> Event ()
leave = silence . bind "mouseleave"

-- | Element loses focus.
blur :: Element -> Event ()
blur = silence . bind "blur"

-- Drag events and support functions

-- | Enables drag on an element.
allowDrag :: Element -> IO Element
allowDrag = setDraggable True

-- | Disables drag on an element.
blockDrag :: Element -> IO Element
blockDrag = setDraggable False

-- | Enables or disables drag based on boolean argument.
setDraggable :: Bool -> Element -> IO Element
setDraggable t = setAttr "draggable" (if t then "true" else "false")

-- | Set the drag data for an element.  This data becomes the EventData for all drag-related events.
setDragData :: String -> Element -> IO Element
setDragData d = setAttr "ondragstart" $ "event.dataTransfer.setData('dragData', '" ++ d ++ "')"

-- | Enables an element to accept drops.
allowDrop :: Element -> IO Element
allowDrop e =
    setAttr "ondragover" "event.preventDefault()" e >>= setAttr "ondrop" "event.preventDefault()"

-- | Disables an element from accepting drops.
blockDrop :: Element -> IO Element
blockDrop e = setAttr "ondragover" "" e >>= setAttr "ondrop" ""

-- | Enables or disables an element from accepting drops based on boolean argument.
setDroppable :: Bool -> Element -> IO Element
setDroppable t = if t then allowDrop else blockDrop

-- | Drag starts.
dragStart :: Element -> Event ()
dragStart = silence . bind "dragstart"

-- | Drag enter.
dragEnter :: Element -> Event ()
dragEnter = silence . bind "dragenter"

-- | Drag over event.
dragOver :: Element -> Event ()
dragOver = silence . bind "dragover"

-- | Drag leave event.
dragLeave :: Element -> Event ()
dragLeave = silence . bind "dragleave"

-- | Drag event.
drag :: Element -> Event ()
drag = silence . bind "drag"

-- | Drop event.
drop :: Element -> Event ()
drop = silence . bind "drop"

-- | Drag end event.
dragEnd :: Element -> Event ()
dragEnd = silence . bind "dragend"
