{-# OPTIONS -fno-warn-wrong-do-bind #-}
module Graphics.UI.Threepenny.JQuery where

import Control.Arrow
import           Data.Aeson                 as JSON
import Data.String
import Data.Char
import Data.Default
import Data.Maybe
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Internal.FFI    (showJSON, readJSON)
import qualified Graphics.UI.Threepenny.Internal.Driver as Core
import qualified Graphics.UI.Threepenny.Internal.Types  as Core
import Reactive.Threepenny


data Easing = Swing | Linear
  deriving (Eq,Enum,Show)

instance Default Easing where
  def = Linear

-- | Animate property changes of a function.
animate :: Element -> [(String,String)] -> Int -> Easing -> IO () -> IO ()
animate el props duration easing complete =
    Core.withElement (toElement el) $ \elid window ->
        callDeferredFunction window
            "jquery_animate"
            [showJSON elid,showJSON propsJSON,show duration,map toLower (show easing)]
            (const complete)
    where
    propsJSON = JSON.object [fromString name .= val | (name,val) <- props]

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
setFocus :: Element -> UI ()
setFocus = runFunction . ffi "$(%1).focus()"

-- | Scroll to the bottom of an element.
scrollToBottom :: Element -> UI ()
scrollToBottom = runFunction . ffi "jquery_scrollToBottom(%1)"
