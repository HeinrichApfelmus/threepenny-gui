
-- | Utility methods for handling particular events.

module Graphics.UI.Threepenny.Events where

import Graphics.UI.Threepenny

-- | Bind an event handler to the click event of the given element.
onClick :: MonadTP m
        => Element             -- ^ The element to bind to.
        -> (EventData -> m ()) -- ^ The event handler.
        -> m ()
onClick = bind "click"

-- | Bind an event handler to the hover event of the given element.
onHover :: MonadTP m
        => Element             -- ^ The element to bind to.
        -> (EventData -> m ()) -- ^ The event handler.
        -> m ()
onHover = bind "mouseenter"

-- | Bind an event handler to the blur event of the given element.
onBlur :: MonadTP m
       => Element             -- ^ The element to bind to.
       -> (EventData -> m ()) -- ^ The event handler.
       -> m ()
onBlur = bind "mouseleave"

-- Drag events and support functions

-- | Enables drag on an element.
allowDrag :: MonadTP m => Element -> m Element
allowDrag = setDraggable True

-- | Disables drag on an element.
blockDrag :: MonadTP m => Element -> m Element
blockDrag = setDraggable False

-- | Enables or disables drag based on boolean argument.
setDraggable :: MonadTP m => Bool -> Element -> m Element
setDraggable t = setAttr "draggable" (if t then "true" else "false")

-- | Set the drag data for an element.  This data becomes the EventData for all drag-related events.
setDragData :: MonadTP m => String -> Element -> m Element
setDragData d = setAttr "ondragstart" $ "event.dataTransfer.setData('dragData', '" ++ d ++ "')"

-- | Enables an element to accept drops.
allowDrop :: MonadTP m => Element -> m Element
allowDrop e =
    setAttr "ondragover" "event.preventDefault()" e >>= setAttr "ondrop" "event.preventDefault()"

-- | Disables an element from accepting drops.
blockDrop :: MonadTP m => Element -> m Element
blockDrop e = setAttr "ondragover" "" e >>= setAttr "ondrop" ""

-- | Enables or disables an element from accepting drops based on boolean argument.
setDroppable :: MonadTP m => Bool -> Element -> m Element
setDroppable t = if t then allowDrop else blockDrop

-- | Bind an event handler to the drag start event.
onDragStart :: MonadTP m => Element -> (EventData -> m ()) -> m ()
onDragStart = bind "dragstart"

-- | Bind an event handler to the drag enter event.
onDragEnter :: MonadTP m => Element -> (EventData -> m ()) -> m ()
onDragEnter = bind "dragenter"

-- | Bind an event handler to the drag over event.
onDragOver :: MonadTP m => Element -> (EventData -> m ()) -> m ()
onDragOver = bind "dragover"

-- | Bind an event handler to the drag leave event.
onDragLeave :: MonadTP m => Element -> (EventData -> m ()) -> m ()
onDragLeave = bind "dragleave"

-- | Bind an event handler to the drag event.
onDrag :: MonadTP m => Element -> (EventData -> m ()) -> m ()
onDrag = bind "drag"

-- | Bind an event handler to the drop event.
onDrop :: MonadTP m => Element -> (EventData -> m ()) -> m ()
onDrop = bind "drop"

-- | Bind an event handler to the drag end event.
onDragEnd :: MonadTP m => Element -> (EventData -> m ()) -> m ()
onDragEnd = bind "dragend"