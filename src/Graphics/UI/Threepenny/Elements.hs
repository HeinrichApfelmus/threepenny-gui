-- | Predefined DOM elements, for convenience.
module Graphics.UI.Threepenny.Elements where

import Control.Monad
import Prelude hiding (div)
import Graphics.UI.Threepenny.Core

-- | Add a stylesheet to the head.
--
-- The second argument refers to the filename of the stylesheet,
-- but not its complete filepath.
-- Threepenny will prefix the 'css' subdirectory of the 'tpStatic' configuration field
-- to construct the complete filepath.
addStyleSheet
    :: Window
    -> FilePath
    -> IO ()
addStyleSheet w filename = void $ do
  head <- getHead w
  (newElement w "link"
    # set (attr "rel" ) "stylesheet"
    # set (attr "type") "text/css"
    # set (attr "href") ("/static/css/" ++ filename))
    # appendTo head

-- | Make a new anchor, @a@.
anchor :: Window -> IO Element
anchor = flip newElement "a"

-- | Make a new button.
button :: Window -> IO Element
button = flip newElement "button"

-- | Make a new @div@ element.
div :: Window -> IO Element
div = flip newElement "div"

-- | Make a new form.
form :: Window -> IO Element
form = flip newElement "form"

-- | Make a new heading of first level, @h1@.
h1 :: Window -> IO Element
h1 = flip newElement "h1"

-- | Make a new heading of second level, @h2@.
h2 :: Window -> IO Element
h2 = flip newElement "h2"

-- | Make a new horizontal rule, @hr@.
hr :: Window -> IO Element
hr = flip newElement "hr"

-- | Make a new image, @img@.
img :: Window -> IO Element
img = flip newElement "img"

-- | Make a new input element.
input :: Window -> IO Element
input = flip newElement "input"

-- | Make a new label.
label :: Window -> IO Element
label = flip newElement "label"

-- | Make a new list item, @li@.
li :: Window -> IO Element
li = flip newElement "li"

-- | Make a new @div@ element.
new :: Window -> IO Element
new = flip newElement "div"

-- | Make a new paragraph, @p@.
paragraph :: Window -> IO Element
paragraph = flip newElement "p"

-- | Make a new span.
span :: Window -> IO Element
span = flip newElement "span"

-- | Make a new table, @table@.
table :: Window -> IO Element
table = flip newElement "table"

-- | Make a new table row, @tr@.
tableRow :: Window -> IO Element
tableRow = flip newElement "tr"

-- | Make a new table cell, @td@.
tableCell :: Window -> IO Element
tableCell = flip newElement "td"

-- | Make a new textarea.
textarea :: Window -> IO Element
textarea = flip newElement "textarea"

-- | Make a new unordered list, @ul@.
ul :: Window -> IO Element
ul = flip newElement "ul"
