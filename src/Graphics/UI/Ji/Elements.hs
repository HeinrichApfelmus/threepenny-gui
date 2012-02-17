-- | Important DOM elements, for convenience.

module Graphics.UI.Ji.Elements where

import Prelude hiding (head)
import Graphics.UI.Ji
import Graphics.UI.Ji.DOM

-- | Make a new anchor.
newAnchor :: MonadJi m => m Element
newAnchor = newElement "a"

-- | Make a new form.
newForm :: MonadJi m => m Element
newForm = newElement "form"

-- | Make a new label.
newLabel :: MonadJi m => m Element
newLabel = newElement "label"

-- | Make a new input.
newInput :: MonadJi m => m Element
newInput = newElement "input"

-- | Make a new textarea.
newTextarea :: MonadJi m => m Element
newTextarea = newElement "textarea"

-- | Make a new table.
newTable :: MonadJi m => m Element
newTable = newElement "table"

-- | Make a new row.
newRow :: MonadJi m => m Element
newRow = newElement "tr"

-- | Make a new data.
newData :: MonadJi m => m Element
newData = newElement "td"

-- | Make a new img.
newImg :: MonadJi m => m Element
newImg = newElement "img"

-- | Add a stylesheet to the head.
addStyleSheet :: MonadJi m => FilePath -> m ()
addStyleSheet filename = do
  head <- getHead
  newElement "link"
    # setAttr "rel" "stylesheet"
    # setAttr "type" "text/css"
    # setAttr "href" ("/static/css/" ++ filename)
    # appendTo head
    # unit

-- | Add a clear.
addClear :: MonadJi m => Element -> m ()
addClear el = do
  new #. "clear" #+ el # unit
