module Graphics.UI.Ji.JQuery where

import Control.Arrow
import Data.Default
import Data.String.Extra
import Graphics.UI.Ji
import Text.JSON

data Easing = Swing | Linear
  deriving (Eq,Enum,Show)

instance Default Easing where
  def = Linear

-- | Animate property changes of a function.
animate :: MonadJi m => Element -> [(String,String)] -> Int -> Easing -> m () -> m ()
animate el props duration easing complete = do
  callDeferredFunction
    "jquery_animate"
    [encode el,encode (makeObj (map (second showJSON) props)),show duration,lower (show easing)]
    (const complete)

-- | Fade in an element.
fadeIn :: MonadJi m => Element -> Int -> Easing -> m () -> m ()
fadeIn el duration easing complete = animate el [("opacity","1")] duration easing complete

-- | Fade out an element.
fadeOut :: MonadJi m => Element -> Int -> Easing -> m () -> m ()
fadeOut el duration easing complete = animate el [("opacity","0")] duration easing complete
