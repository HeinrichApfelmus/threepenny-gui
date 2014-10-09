-- | Predefined DOM elements, for convenience.
module Graphics.UI.Threepenny.Elements (
    -- * Combinations and utilities
    addStyleSheet,
    -- text,
    new,

    -- * Primitive HTML elements
    address, a, anchor, applet, area, audio,
    basefont, big, blockquote, body, bold, br, button,
    canvas, caption, center, cite, code,
    ddef, define, div, dlist,
    dterm, emphasize, fieldset, font, form, frame, frameset,
    h1, h2, h3, h4, h5, h6, header, hr,
    img, image, input, italics,
    keyboard, label, legend, li, link, map, meta, noframes, olist, option,
    p, paragraph, param, pre,
    sample, select, small, source, span, strong, sub, sup,
    table, td, textarea, th, thebase,
    thehtml, title_, tr, tt, ul,
    underline, variable, video,
    ) where

import           Control.Monad
import           Control.Monad.Trans.Reader
import           Graphics.UI.Threepenny.Core
import           Prelude                     hiding (div, map, span)

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
    -> UI ()
addStyleSheet w filename = void $ do
    el <- mkElement "link"
            # set (attr "rel" ) "stylesheet"
            # set (attr "type") "text/css"
            # set (attr "href") ("/static/css/" ++ filename)
    getHead w #+ [element el]

-- | Make a new @div@ element, synonym for 'div'.
new :: UI Element
new = div

{-----------------------------------------------------------------------------
    Primitives

    Taken from the HTML library (BSD3 license)
    http://hackage.haskell.org/package/html
------------------------------------------------------------------------------}
tag    = mkElement
itag   = mkElement

address             =  tag "address"
a                   =  anchor
anchor              =  tag "a"
applet              =  tag "applet"
area                = itag "area"
audio               =  tag "audio"
basefont            = itag "basefont"
big                 =  tag "big"
blockquote          =  tag "blockquote"
body                =  tag "body"
bold                =  tag "b"
br                  = itag "br"
button              =  tag "button"
canvas              =  tag "canvas"
caption             =  tag "caption"
center              =  tag "center"
cite                =  tag "cite"
code                =  tag "code"
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
label               =  tag "label"
legend              =  tag "legend"
li                  =  tag "li"
link                =  tag "link"
map                 =  tag "map"
meta                = itag "meta"
noframes            =  tag "noframes"
olist               =  tag "ol"
option              =  tag "option"
p                   =  tag "p"
paragraph           =  tag "p"
param               = itag "param"
pre                 =  tag "pre"
sample              =  tag "samp"
select              =  tag "select"
small               =  tag "small"
source              =  tag "source"
strong              =  tag "strong"
sub                 =  tag "sub"
sup                 =  tag "sup"
table               =  tag "table"
td                  =  tag "td"
textarea            =  tag "textarea"
th                  =  tag "th"
thebase             = itag "base"
thehtml             =  tag "html"
span                =  tag "span"
title_              =  tag "title"
tr                  =  tag "tr"
tt                  =  tag "tt"
ul                  =  tag "ul"
underline           =  tag "u"
variable            =  tag "var"
video               =  tag "video"
