module Graphics.UI.Threepenny.Events (
    -- * Synopsis
    -- | Events on DOM elements.

    -- * Convenience events
    valueChange, selectionChange, checkedChange,

    -- * Standard DOM events
    click, contextmenu, mousemove, mousedown, mouseup,
    hover, leave,
    focus, blur,
    KeyCode, keyup, keydown, keypress,
    ) where

import Graphics.UI.Threepenny.Attributes
import Graphics.UI.Threepenny.Core

silence = fmap (const ())

{-----------------------------------------------------------------------------
    Events
------------------------------------------------------------------------------}
-- | Event that occurs when the /user/ changes the value of the input element.
valueChange :: Element -> Event String
valueChange el = unsafeMapUI el (const $ get value el) (domEvent "keydown" el)

unsafeMapUI el f = unsafeMapIO (\a -> getWindow el >>= \w -> runUI w (f a))

-- | Event that occurs when the /user/ changes the selection of a @<select>@ element.
selectionChange :: Element -> Event (Maybe Int)
selectionChange el = unsafeMapUI el (const $ get selection el) (click el)

-- | Event that occurs when the /user/ changes the checked status of an input
-- element of type checkbox.
checkedChange :: Element -> Event Bool
checkedChange el = unsafeMapUI el (const $ get checked el) (click el)

{-----------------------------------------------------------------------------
    DOM Events
------------------------------------------------------------------------------}
-- | Mouse click.
click :: Element -> Event ()
click = silence . domEvent "click"

-- | Context menu event.
contextmenu :: Element -> Event (Double,Double)
contextmenu = fmap readCoordinates . domEvent "contextmenu"

-- | Mouse enters an element.
hover :: Element -> Event ()
hover = silence . domEvent "mouseenter"

-- | Event that periodically occurs while the mouse is moving over an element.
--
-- The event value represents the mouse coordinates
-- relative to the upper left corner of the element.
--
-- Note: The @<body>@ element responds to mouse move events,
-- but only in the area occupied by actual content,
-- not the whole browser window.
mousemove :: Element -> Event (Double,Double)
mousemove = fmap readCoordinates . domEvent "mousemove"

-- NB: we return @Double@s rather than @Int@s here because
-- mouse events' return types have been redefined from @long@
-- to @double@ in the CSS Object Model View Model working draft,
-- which has started to be adopted by browsers.
-- See https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent#Specifications
-- and https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/pageX
--
-- Similarly, we rely on jQuery's @.offset()@ to return
-- coordinates relative to the upper left corner of the
-- element, and this may return fractional data.
-- https://api.jquery.com/offset/
readCoordinates :: EventData -> (Double,Double)
readCoordinates json = (x,y)
    where [x,y] = unsafeFromJSON json

-- | Mouse down event.
-- The mouse coordinates are relative to the element.
mousedown :: Element -> Event (Double,Double)
mousedown = fmap readCoordinates . domEvent "mousedown"

-- | Mouse up event.
-- The mouse coordinates are relative to the element.
mouseup :: Element -> Event (Double,Double)
mouseup = fmap readCoordinates . domEvent "mouseup"

-- | Mouse leaving an element.
leave :: Element -> Event ()
leave = silence . domEvent "mouseleave"

-- | Element receives focus.
focus :: Element -> Event ()
focus = silence . domEvent "focus"

-- | Element loses focus.
blur :: Element -> Event ()
blur = silence . domEvent "blur"


type KeyCode = Int

-- | Key pressed while element has focus.
-- Returns the keycode (as opposed to the ASCII value) of any key, including
-- SHIFT, CTRL and arrow keys.
keydown :: Element -> Event KeyCode
keydown = fmap unsafeFromJSON . domEvent "keydown"

-- | Key released while element has focus.
keyup :: Element -> Event KeyCode
keyup   = fmap unsafeFromJSON . domEvent "keyup"

-- | Key pressed while element has focus.
-- Returns the actual character, taking into account SHIFT or CAPS LOCK.
keypress :: Element -> Event Char
keypress = fmap (toEnum . read . head . unsafeFromJSON) . domEvent "keypress"
