-- | Important DOM elements, for convenience.

module Graphics.UI.Threepenny.Elements where

import Prelude hiding (head)
import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.DOM

-- | Make a new anchor.
newAnchor :: MonadTP m => m Element
newAnchor = newElement "a"

-- | Make a new form.
newForm :: MonadTP m => m Element
newForm = newElement "form"

-- | Make a new label.
newLabel :: MonadTP m => m Element
newLabel = newElement "label"

-- | Make a new input.
newInput :: MonadTP m => m Element
newInput = newElement "input"

-- | Make a new textarea.
newTextarea :: MonadTP m => m Element
newTextarea = newElement "textarea"

-- | Make a new table.
newTable :: MonadTP m => m Element
newTable = newElement "table"

-- | Make a new row.
newRow :: MonadTP m => m Element
newRow = newElement "tr"

-- | Make a new data.
newData :: MonadTP m => m Element
newData = newElement "td"

-- | Make a new img.
newImg :: MonadTP m => m Element
newImg = newElement "img"

-- | Add a stylesheet to the head.
addStyleSheet :: MonadTP m => FilePath -> m ()
addStyleSheet filename = do
  head <- getHead
  newElement "link"
    # setAttr "rel" "stylesheet"
    # setAttr "type" "text/css"
    # setAttr "href" ("/static/css/" ++ filename)
    # appendTo head
    # unit

-- | Add a clear.
addClear :: MonadTP m => Element -> m ()
addClear el = do
  new #. "clear" #+ el # unit
