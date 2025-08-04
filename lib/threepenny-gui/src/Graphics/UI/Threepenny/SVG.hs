module Graphics.UI.Threepenny.SVG (
  -- * Synopsis
  -- | SVG elements and attributes.
  --
  -- It is recommended that you import this module qualified, i.e.
  --
  -- > import qualified Graphics.UI.Threepenny.SVG as SVG
  --
  -- Note that some SVG attributes and elements have the same name.
  -- In this case, only the most common of either has been imported.
  -- If you need the other version, you have to import the individual modules.
  
  module Graphics.UI.Threepenny.SVG.Attributes,
  module Graphics.UI.Threepenny.SVG.Elements
  ) where

import Graphics.UI.Threepenny.SVG.Attributes hiding (path, title, glyphRef)
import Graphics.UI.Threepenny.SVG.Elements   hiding (cursor, filter, mask, style)
