module Graphics.UI.Threepenny.JQuery where

import Control.Arrow
import Data.Char
import Data.Default
import Data.Maybe
import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.Internal.Types
import Text.JSON

data Easing = Swing | Linear
  deriving (Eq,Enum,Show)

instance Default Easing where
  def = Linear

-- | Animate property changes of a function.
animate :: Element -> [(String,String)] -> Int -> Easing -> IO () -> IO ()
animate (Element el window) props duration easing complete = do
  callDeferredFunction window
    "jquery_animate"
    [encode el,encode (makeObj (map (second showJSON) props)),show duration,map toLower (show easing)]
    (const complete)

-- | Fade in an element.
fadeIn :: Element -> Int -> Easing -> IO () -> IO ()
fadeIn el duration easing complete = animate el [("opacity","1")] duration easing complete

-- | Fade out an element.
fadeOut :: Element -> Int -> Easing -> IO () -> IO ()
fadeOut el duration easing complete = animate el [("opacity","0")] duration easing complete

-- | Do something on return.
onSendValue :: Element -> (String -> IO ()) -> IO ()
onSendValue input m = do
  bind "sendvalue" input $ \(EventData evdata) -> do
    m (concat (catMaybes evdata))

-- | Focus an element.
setFocus :: Element -> IO Element
setFocus e@(Element el window) = runFunction window "jquery_setFocus" [encode el] >> return e

-- | Scroll to the bottom of an element.
scrollToBottom :: Element -> IO ()
scrollToBottom (Element area window) = runFunction window "jquery_scrollToBottom" [encode area]
