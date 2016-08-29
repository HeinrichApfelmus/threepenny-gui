module Graphics.UI.Threepenny.JQuery where

import Control.Arrow
import Data.String
import Data.Char
import Data.Default
import Data.Maybe

import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    jQuery utilities
------------------------------------------------------------------------------}
data Easing = Swing | Linear
  deriving (Eq,Enum,Show)

instance Default Easing where def = Linear

-- | Fade in an element.
fadeIn :: Element -> Int -> Easing -> IO () -> UI ()
fadeIn el duration easing complete = do
    callback <- ffiExport complete
    runFunction $ ffi "$(%1).animate({opacity: 1}, %2 * 1, %3, %4)"
        el duration (map toLower (show easing)) callback

-- | Fade out an element.
fadeOut :: Element -> Int -> Easing -> IO () -> UI ()
fadeOut el duration easing complete = do
    callback <- ffiExport complete
    runFunction $ ffi "$(%1).animate({opacity: 0}, %2 * 1, %3, %4)"
        el duration (map toLower (show easing)) callback

-- | The 'sendValue' event happens whenever the return key is pressed
-- while the element has focus. Its data is the event value.
sendValue :: Element -> Event String
sendValue = fmap unsafeFromJSON . domEvent "sendvalue"

-- | Focus an element.
setFocus :: Element -> UI ()
setFocus = runFunction . ffi "$(%1).focus()"

-- | Scroll to the bottom of an element.
scrollToBottom :: Element -> UI ()
scrollToBottom = runFunction . ffi "jquery_scrollToBottom(%1)"
