module Graphics.UI.Threepenny.Events (
    -- * Synopsis
    -- | Events on DOM elements.
    
    -- * Convenience events
    valueChange, selectionChange,
    
    -- * Standard DOM events
    click, mousemove, hover, blur, leave,
    KeyCode, keyup, keydown,
    ) where

import Graphics.UI.Threepenny.Attributes
import Graphics.UI.Threepenny.Core

silence = fmap (const ())

{-----------------------------------------------------------------------------
    Events
------------------------------------------------------------------------------}
-- | Event that occurs when the /user/ changes the value of the input element.
valueChange :: Element -> Event String
valueChange el = unsafeMapIO (const $ get value el) (domEvent "keydown" el)

-- | Event that occurs when the /user/ changes the selection of a @<select>@ element.
selectionChange :: Element -> Event (Maybe Int)
selectionChange el = unsafeMapIO (const $ get selection el) (click el)

{-----------------------------------------------------------------------------
    DOM Events
------------------------------------------------------------------------------}
-- | Mouse click.
click :: Element -> Event ()
click = silence . domEvent "click"

-- | Mouse hovering over an element.
hover :: Element -> Event ()
hover = silence . domEvent "mouseenter"

-- | Event that periodically occurs while the mouse is moving over an element.
--
-- The event value represents the mouse coordinates
-- relative to the upper left corner of the entire page.
--
-- Note: The @<body>@ element responds to mouse move events,
-- but only in the area occupied by actual content,
-- not the whole browser window.
mousemove :: Element -> Event (Int,Int)
mousemove = fmap readPair . domEvent "mousemove"
    where readPair (EventData (Just x:Just y:_)) = (read x, read y)

-- | Mouse leaving an element.
leave :: Element -> Event ()
leave = silence . domEvent "mouseleave"

-- | Element loses focus.
blur :: Element -> Event ()
blur = silence . domEvent "blur"


type KeyCode = Int

-- | Key pressed while element has focus.
keydown :: Element -> Event KeyCode
keydown = fmap read1  . domEvent "keydown"

-- | Key released while element has focus.
keyup :: Element -> Event KeyCode
keyup   = fmap read1 . domEvent "keyup"

read1 (EventData (Just s:_)) = read s
