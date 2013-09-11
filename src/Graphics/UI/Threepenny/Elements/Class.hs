
module Graphics.UI.Threepenny.Elements.Class (
    Widget (..)
  ) where

import Graphics.UI.Threepenny

class Widget e where
  toElement :: e -> IO Element
