
-- | Utility methods for handling particular events.

module Graphics.UI.Threepenny.Events where

import Graphics.UI.Threepenny

-- | Bind an event handler to the click event of the given element.
onClick :: Element -> (EventData -> IO ()) -> IO ()
onClick = bind "click"

-- | Bind an event handler to the hover event of the given element.
onHover :: Element -> (EventData -> IO ()) -> IO ()
onHover = bind "mouseenter"

-- | Bind an event handler to the blur event of the given element.
onBlur :: Element -> (EventData -> IO ()) -> IO ()
onBlur = bind "mouseleave"

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

-- | Bind an event handler to the drag start event.
onDragStart :: Element -> (EventData -> IO ()) -> IO ()
onDragStart = bind "dragstart"

-- | Bind an event handler to the drag enter event.
onDragEnter :: Element -> (EventData -> IO ()) -> IO ()
onDragEnter = bind "dragenter"

-- | Bind an event handler to the drag over event.
onDragOver :: Element -> (EventData -> IO ()) -> IO ()
onDragOver = bind "dragover"

-- | Bind an event handler to the drag leave event.
onDragLeave :: Element -> (EventData -> IO ()) -> IO ()
onDragLeave = bind "dragleave"

-- | Bind an event handler to the drag event.
onDrag :: Element -> (EventData -> IO ()) -> IO ()
onDrag = bind "drag"

-- | Bind an event handler to the drop event.
onDrop :: Element -> (EventData -> IO ()) -> IO ()
onDrop = bind "drop"

-- | Bind an event handler to the drag end event.
onDragEnd :: Element -> (EventData -> IO ()) -> IO ()
onDragEnd = bind "dragend"
