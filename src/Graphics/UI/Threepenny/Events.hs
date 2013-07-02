module Graphics.UI.Threepenny.Events (
    -- * Synopsis
    -- | Common DOM events, for convenience.
    
    -- * Documentation
    click, hover, blur, leave,
    ) where

import Graphics.UI.Threepenny.Core

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
