module Graphics.UI.Threepenny.Events (
    -- * Synopsis
    -- | Common DOM events, for convenience.
    
    -- * Documentation
    click, mousemove, hover, blur, leave,
    keyup, keydown,
    ) where

import Graphics.UI.Threepenny.Core

silence = fmap (const ())

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
