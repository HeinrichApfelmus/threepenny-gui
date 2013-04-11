-- | Important DOM elements, for convenience.

module Graphics.UI.Threepenny.Elements where

import Prelude hiding (head)
import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.DOM

-- | Make a new anchor.
newAnchor :: Window -> IO Element
newAnchor w = newElement w "a"

-- | Make a new form.
newForm :: Window -> IO Element
newForm w = newElement w "form"

-- | Make a new label.
newLabel :: Window -> IO Element
newLabel w = newElement w "label"

-- | Make a new input.
newInput :: Window -> IO Element
newInput w = newElement w "input"

-- | Make a new textarea.
newTextarea :: Window -> IO Element
newTextarea w = newElement w "textarea"

-- | Make a new table.
newTable :: Window -> IO Element
newTable w = newElement w "table"

-- | Make a new row.
newRow :: Window -> IO Element
newRow w = newElement w "tr"

-- | Make a new data.
newData :: Window -> IO Element
newData w = newElement w "td"

-- | Make a new img.
newImg :: Window -> IO Element
newImg w = newElement w "img"

-- | Add a stylesheet to the head.
addStyleSheet :: Window -> FilePath -> IO ()
addStyleSheet w filename = do
  head <- getHead w
  newElement w "link"
    # setAttr "rel" "stylesheet"
    # setAttr "type" "text/css"
    # setAttr "href" ("/static/css/" ++ filename)
    # appendTo head
    # unit

-- | Add a clear.
addClear :: Window -> Element -> IO ()
addClear w el = do
  new w #. "clear" #+ el # unit
