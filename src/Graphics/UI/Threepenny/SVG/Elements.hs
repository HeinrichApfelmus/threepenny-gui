module Graphics.UI.Threepenny.SVG.Elements (
    -- * Synopsis
    -- | SVG elements as defined by W3C, Scalable Vector Graphics (SVG) 1.1
    --   (Second Edition) Appendix M <http://www.w3.org/TR/2011/REC-SVG11-20110816/>.
    -- 
    -- Whenever possible, the Haskell identifier for an element is the 
    -- same as the attribute name. However, sometimes changes are necessary:
    --
    --     1. Hyphens @-@ are replaced by underscores @_@, e.g. 'font_face'.

    -- * SVG Elements
    a, altGlyph, altGlyphDef, altGlyphItem,
    animate, animateColor, animateMotion, animateTransform,
    circle, clipPath, colorProfile, cursor,
    defs, desc,
    ellipse,
    feBlend, feColorMatrix, feComponentTransfer, feComposite, feConvolveMatrix,
    feDiffuseLighting, feDisplacementMap, feDistantLight, feFlood,
    feFuncA, feFuncB, feFuncG, feFuncR,
    feGaussianBlur, feImage, feMerge, feMergeNode, feMorphology, feOffset,
    fePointLight, feSpecularLighting, feSpotLight, feTile, feTurbulence,
    filter, font,
    font_face, font_face_format, font_face_name, font_face_srv, font_face_uri,
    foreignObject,
    g, glyph, glyphRef,
    hkern,
    image,
    line, linearGradient,
    marker, mask, metadata, missing_glyph, mpath,
    path, pattern, polygon, polyline,
    radialGradient, rect,
    script, set, stop, style, svg, switch, symbol,
    text, textPath, title, tref, tspan,
    use,
    view, vkern,
    ) where

import Prelude (Maybe(..))
import Graphics.UI.Threepenny.Core (mkElementNamespace)


tag    = mkElementNamespace (Just "http://www.w3.org/2000/svg")
itag   = tag

a                     =  tag "a"
altGlyph              =  tag "altGlyph"
altGlyphDef           = itag "altGlyphDef"
altGlyphItem          = itag "altGlyphItem"
animate               =  tag "animate"
animateColor          =  tag "animateColor"
animateMotion         =  tag "animateMotion"
animateTransform      = itag "animateTransform"
circle                = itag "circle"
clipPath              =  tag "clipPath"
colorProfile          = itag "colorProfile"
cursor                = itag "cursor"
defs                  =  tag "defs"
desc                  = itag "desc"
ellipse               =  tag "ellipse"
feBlend               =  tag "feBlend"
feColorMatrix         =  tag "feColorMatrix"
feComponentTransfer   =  tag "feComponentTransfer"
feComposite           =  tag "feComposite"
feConvolveMatrix      =  tag "feConvolveMatrix"
feDiffuseLighting     =  tag "feDiffuseLighting"
feDisplacementMap     =  tag "feDisplacementMap"
feDistantLight        = itag "feDistantLight"
feFlood               =  tag "feFlood"
feFuncA               = itag "feFuncA"
feFuncB               = itag "feFuncB"
feFuncG               = itag "feFuncG"
feFuncR               = itag "feFuncR"
feGaussianBlur        =  tag "feGaussianBlur"
feImage               =  tag "feImage"
feMerge               =  tag "feMerge"
feMergeNode           = itag "feMergeNode"
feMorphology          =  tag "feMorphology"
feOffset              =  tag "feOffset"
fePointLight          = itag "fePointLight"
feSpecularLighting    =  tag "feSpecularLighting"
feSpotLight           = itag "feSpotLight"
feTile                =  tag "feTile"
feTurbulence          =  tag "feTurbulence"
filter                =  tag "filter"
font                  =  tag "font"
font_face             = itag "font-face"
font_face_format      = itag "font-face-format"
font_face_name        = itag "font-face-name"
font_face_srv         = itag "font-face-src"
font_face_uri         = itag "font-face-uri"
foreignObject         =  tag "foreignObject"
g                     =  tag "g"
glyph                 =  tag "glyph"
glyphRef              =  tag "glyphRef"
hkern                 = itag "hkern"
image                 =  tag "image"
line                  =  tag "line"
linearGradient        =  tag "linearGradient"
marker                =  tag "marker"
mask                  =  tag "mask"
metadata              = itag "metadata"
missing_glyph         =  tag "missing-glyph"
mpath                 = itag "mpath"
path                  = itag "path"
pattern               =  tag "pattern"
polygon               =  tag "polygon"
polyline              =  tag "polyline"
radialGradient        =  tag "radialGradient"
rect                  =  tag "rect"
script                = itag "script"
set                   = itag "set"
stop                  =  tag "stop"
style                 = itag "style"
svg                   =  tag "svg"
switch                =  tag "switch"
symbol                =  tag "symbol"
text                  =  tag "text"
textPath              =  tag "textPath"
title                 = itag "title"
tref                  =  tag "tref"
tspan                 =  tag "tspan"
use                   =  tag "use"
view                  = itag "view"
vkern                 = itag "vkern"
