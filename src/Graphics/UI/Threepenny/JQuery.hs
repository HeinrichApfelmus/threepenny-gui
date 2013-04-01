module Graphics.UI.Threepenny.JQuery where

import Control.Arrow
import Data.Char
import Data.Default
import Data.Maybe
import Graphics.UI.Threepenny
import Text.JSON

data Easing = Swing | Linear
  deriving (Eq,Enum,Show)

instance Default Easing where
  def = Linear

-- | Animate property changes of a function.
animate :: MonadTP m => Element -> [(String,String)] -> Int -> Easing -> m () -> m ()
animate el props duration easing complete = do
  callDeferredFunction
    "jquery_animate"
    [encode el,encode (makeObj (map (second showJSON) props)),show duration,map toLower (show easing)]
    (const complete)

-- | Fade in an element.
fadeIn :: MonadTP m => Element -> Int -> Easing -> m () -> m ()
fadeIn el duration easing complete = animate el [("opacity","1")] duration easing complete

-- | Fade out an element.
fadeOut :: MonadTP m => Element -> Int -> Easing -> m () -> m ()
fadeOut el duration easing complete = animate el [("opacity","0")] duration easing complete

-- | Do something on return.
onSendValue :: (MonadTP m) => Element -> (String -> m ()) -> m ()
onSendValue input m = do
  bind "sendvalue" input $ \(EventData evdata) -> do
    m (concat (catMaybes evdata))

-- | Focus an element.
setFocus :: (MonadTP m) => Element -> m Element
setFocus el = runFunction "jquery_setFocus" [encode el] >> return el

-- | Scroll to the bottom of an element.
scrollToBottom :: MonadTP m => Element -> m ()
scrollToBottom area = runFunction "jquery_scrollToBottom" [encode area]
