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

-- | Drag starts.
dragStart :: Element -> Event ()
dragStart = silence . domEvent "dragstart"

-- | Drag enter.
dragEnter :: Element -> Event ()
dragEnter = silence . domEvent "dragenter"

-- | Drag over event.
dragOver :: Element -> Event ()
dragOver = silence . domEvent "dragover"

-- | Drag leave event.
dragLeave :: Element -> Event ()
dragLeave = silence . domEvent "dragleave"

-- | Drag event.
drag :: Element -> Event ()
drag = silence . domEvent "drag"

-- | Drop event.
drop :: Element -> Event ()
drop = silence . domEvent "drop"

-- | Drag end event.
dragEnd :: Element -> Event ()
dragEnd = silence . domEvent "dragend"
