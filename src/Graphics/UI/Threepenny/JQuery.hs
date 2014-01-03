{-# OPTIONS -fno-warn-wrong-do-bind #-}
module Graphics.UI.Threepenny.JQuery where

import Control.Arrow
import Data.String
import Data.Char
import Data.Default
import Data.Maybe
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Internal.Driver as Core
import qualified Graphics.UI.Threepenny.Internal.Types  as Core
import Reactive.Threepenny


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

-- | Do something on return.
sendValue :: Element -> Event String
sendValue el = fmap f (domEvent "sendvalue" el)
    where
    f (EventData x) = concat . catMaybes $ x

-- | Focus an element.
setFocus :: Element -> UI ()
setFocus = runFunction . ffi "$(%1).focus()"

-- | Scroll to the bottom of an element.
scrollToBottom :: Element -> UI ()
scrollToBottom = runFunction . ffi "jquery_scrollToBottom(%1)"
