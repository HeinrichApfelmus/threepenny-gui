{-# OPTIONS -fno-warn-wrong-do-bind #-}
module Graphics.UI.Threepenny.JQuery where

import Control.Arrow
import Data.Char
import Data.Default
import Data.Maybe
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Internal.Core as Core
import qualified Graphics.UI.Threepenny.Internal.Types as Core
import Text.JSON
import Reactive.Threepenny

data Easing = Swing | Linear
  deriving (Eq,Enum,Show)

instance Default Easing where
  def = Linear

-- | Animate property changes of a function.
animate :: Element -> [(String,String)] -> Int -> Easing -> IO () -> IO ()
animate element props duration easing complete =
    flip updateElement element $ \e -> Core.withElement e $ \elid window ->
        callDeferredFunction window
            "jquery_animate"
            [encode elid,encode (makeObj (map (second showJSON) props)),show duration,map toLower (show easing)]
            (const complete)

-- | Fade in an element.
fadeIn :: Element -> Int -> Easing -> IO () -> IO ()
fadeIn el duration easing complete =
    animate el [("opacity","1")] duration easing complete

-- | Fade out an element.
fadeOut :: Element -> Int -> Easing -> IO () -> IO ()
fadeOut el duration easing complete =
    animate el [("opacity","0")] duration easing complete

-- | Do something on return.
sendValue :: Element -> Event String
sendValue el = fmap f (domEvent "sendvalue" el)
    where
    f (EventData x) = concat . catMaybes $ x

-- | Focus an element.
setFocus :: Element -> IO ()
setFocus = updateElementWindow $ \e window ->
    runFunction window (ffi "$(%1).focus()" e)

-- | Scroll to the bottom of an element.
scrollToBottom :: Element -> IO ()
scrollToBottom = updateElementWindow $ \e window ->
    runFunction window (ffi "jquery_scrollToBottom(%1)" e)
