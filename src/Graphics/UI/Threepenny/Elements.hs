-- | Predefined DOM elements, for convenience.
module Graphics.UI.Threepenny.Elements (
    -- * Combinations and utilities
    addStyleSheet,
    -- text,
    new,
    
    -- * Primitive HTML elements
    address, a, anchor, applet, area, basefont, big, blockquote,
    body, bold, br, caption, center, cite, ddef, define, div, dlist,
    dterm, emphasize, fieldset, font, form, frame, frameset,
    h1, h2, h3, h4, h5, h6, header, hr, img, image, input,
    italics, keyboard, legend, li, meta, noframes, olist, option,
    paragraph, param, pre, sample, select, small, span, strong,
    sub, sup, table, td, textarea, th, thebase, thecode,
    thehtml, thelink, themap, thetitle, tr, tt, ul,
    underline, variable, 
    ) where

import Control.Monad
import Control.Monad.Trans.Reader
import Prelude hiding (span,div)
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Combinations
------------------------------------------------------------------------------}
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
    newElement w "link"
        # set (attr "rel" ) "stylesheet"
        # set (attr "type") "text/css"
        # set (attr "href") ("/static/css/" ++ filename)
        # appendTo head

-- Make a @span@ element with a given text content.
-- text :: String -> Dom Element
-- text s = ReaderT $ \w -> newElement w "span" # set text s

-- | Make a new @div@ element, synonym for 'div'.
new :: [Dom Element] -> Dom Element
new = div

{-----------------------------------------------------------------------------
    Primitives
    
    Taken from the HTML library (BSD3 license)
    http://hackage.haskell.org/package/html
------------------------------------------------------------------------------}
tag    = mkElement
itag s = mkElement s []

address             =  tag "address"
a                   =  anchor
anchor              =  tag "a"
applet              =  tag "applet"
area                = itag "area"
basefont            = itag "basefont"
big                 =  tag "big"
blockquote          =  tag "blockquote"
body                =  tag "body"
bold                =  tag "b"
br                  = itag "br"
caption             =  tag "caption"
center              =  tag "center"
cite                =  tag "cite"
ddef                =  tag "dd"
define              =  tag "dfn"
div                 =  tag "div"
dlist               =  tag "dl"
dterm               =  tag "dt"
emphasize           =  tag "em"
fieldset            =  tag "fieldset"
font                =  tag "font"
form                =  tag "form"
frame               =  tag "frame"
frameset            =  tag "frameset"
h1                  =  tag "h1"
h2                  =  tag "h2"
h3                  =  tag "h3"
h4                  =  tag "h4"
h5                  =  tag "h5"
h6                  =  tag "h6"
header              =  tag "head"
hr                  = itag "hr"
img                 = image
image               = itag "img"
input               = itag "input"
italics             =  tag "i"
keyboard            =  tag "kbd"
legend              =  tag "legend"
li                  =  tag "li"
meta                = itag "meta"
noframes            =  tag "noframes"
olist               =  tag "ol"
option              =  tag "option"
paragraph           =  tag "p"
param               = itag "param"
pre                 =  tag "pre"
sample              =  tag "samp"
select              =  tag "select"
small               =  tag "small"
strong              =  tag "strong"
sub                 =  tag "sub"
sup                 =  tag "sup"
table               =  tag "table"
td                  =  tag "td"
textarea            =  tag "textarea"
th                  =  tag "th"
thebase             = itag "base"
thecode             =  tag "code"
thehtml             =  tag "html"
thelink             =  tag "link"
themap              =  tag "map"
span                =  tag "span"
thetitle            =  tag "title"
tr                  =  tag "tr"
tt                  =  tag "tt"
ul                  =  tag "ul"
underline           =  tag "u"
variable            =  tag "var"
