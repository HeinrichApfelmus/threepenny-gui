module Graphics.UI.Threepenny.Attributes (
    -- * Synopsis
    -- | Element attributes.

    -- * Input elements
    checked, selection, enabled,

    -- * HTML attributes
    action, align, alink, alt, altcode, archive,
    background, base, bgcolor, border, bordercolor,
    cellpadding, cellspacing, checked_, class_, clear_, code_, codebase,
    color, cols, colspan, compact, content, coords,
    enctype, face, for, frameborder, height, href, hspace, httpequiv,
    id_, ismap, lang, marginheight, marginwidth, maxlength, method, multiple,
    name, nohref, noresize, noshade, nowrap,
    rel, rev, rows, rowspan, rules,
    scrolling, selected, shape, size, src,
    target, text_, title__, type_, usemap, valign, version, vlink, vspace, width,
    ) where

import qualified Data.Aeson                  as JSON
import           Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Attributes
------------------------------------------------------------------------------}
-- | The @checked@ status of an input element of type checkbox.
checked :: Attr Element Bool
checked = fromJQueryProp "checked" (== JSON.Bool True) JSON.Bool

-- | The @enabled@ status of an input element
enabled :: Attr Element Bool
enabled = fromJQueryProp "disabled" (== JSON.Bool False) (JSON.Bool . not)

-- | Index of the currently selected option of a @<select>@ element.
--
-- The index starts at @0@.
-- If no option is selected, then the selection is 'Nothing'.
selection :: Attr Element (Maybe Int)
selection = fromJQueryProp "selectedIndex" from (JSON.toJSON . maybe (-1) id)
    where
    from s = let JSON.Success x = JSON.fromJSON s in
        if x == -1 then Nothing else Just x


{-----------------------------------------------------------------------------
    HTML atributes

    Taken from the HTML library (BSD3 license)
    http://hackage.haskell.org/package/html
------------------------------------------------------------------------------}
strAttr :: String -> WriteAttr Element String
strAttr name = mkWriteAttr (set' (attr name))

intAttr :: String -> WriteAttr Element Int
intAttr name = mkWriteAttr (set' (attr name) . show)

emptyAttr :: String -> WriteAttr Element Bool
emptyAttr name = mkWriteAttr (set' (attr name) . f)
    where
    f True  = "1"
    f False = "0"

action              =   strAttr "action"
align               =   strAttr "align"
alink               =   strAttr "alink"
alt                 =   strAttr "alt"
altcode             =   strAttr "altcode"
archive             =   strAttr "archive"
background          =   strAttr "background"
base                =   strAttr "base"
bgcolor             =   strAttr "bgcolor"
border              =   intAttr "border"
bordercolor         =   strAttr "bordercolor"
cellpadding         =   intAttr "cellpadding"
cellspacing         =   intAttr "cellspacing"
checked_            = emptyAttr "checked"
clear_              =   strAttr "clear"
code_               =   strAttr "code"
codebase            =   strAttr "codebase"
color               =   strAttr "color"
cols                =   strAttr "cols"
colspan             =   intAttr "colspan"
compact             = emptyAttr "compact"
content             =   strAttr "content"
coords              =   strAttr "coords"
enctype             =   strAttr "enctype"
face                =   strAttr "face"
for                 =   strAttr "for"
frameborder         =   intAttr "frameborder"
height              =   intAttr "height"
href                =   strAttr "href"
hspace              =   intAttr "hspace"
httpequiv           =   strAttr "http-equiv"
id_                 =   strAttr "id"
ismap               = emptyAttr "ismap"
lang                =   strAttr "lang"
marginheight        =   intAttr "marginheight"
marginwidth         =   intAttr "marginwidth"
maxlength           =   intAttr "maxlength"
method              =   strAttr "method"
multiple            = emptyAttr "multiple"
name                =   strAttr "name"
nohref              = emptyAttr "nohref"
noresize            = emptyAttr "noresize"
noshade             = emptyAttr "noshade"
nowrap              = emptyAttr "nowrap"
rel                 =   strAttr "rel"
rev                 =   strAttr "rev"
rows                =   strAttr "rows"
rowspan             =   intAttr "rowspan"
rules               =   strAttr "rules"
scrolling           =   strAttr "scrolling"
selected            = emptyAttr "selected"
shape               =   strAttr "shape"
size                =   strAttr "size"
src                 =   strAttr "src"
target              =   strAttr "target"
text_               =   strAttr "text"
class_              =   strAttr "class"
type_               =   strAttr "type"
title__             =   strAttr "title" -- ugly, but necessary to avoid conflicts with the window title and the title element
usemap              =   strAttr "usemap"
valign              =   strAttr "valign"
version             =   strAttr "version"
vlink               =   strAttr "vlink"
vspace              =   intAttr "vspace"
width               =   intAttr "width"
