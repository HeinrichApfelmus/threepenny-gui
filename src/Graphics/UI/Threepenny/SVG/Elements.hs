--------------------------------------------------------------------------------
-- |
-- Module      : Graphics.UI.Threepenny.SVG.Elements
-- Description : Elements used in SVG markup
--
--  Elements are defined by W3C, Scalable Vector Graphics (SVG) 1.1
--    (Second Edition) Appendix M <http://www.w3.org/TR/2011/REC-SVG11-20110816/>.
--
--------------------------------------------------------------------------------

module Graphics.UI.Threepenny.SVG.Elements (
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
    filter_, font,  fontface, fontfaceformat, fontfacename, fontfacesrv,
    fontfaceuri, foreignObject,
    g, glyph, glyphRef,
    hkern,
    image,
    line, linearGradient,
    marker, mask, metadata, missingglyph, mpath,
    path, pattern, polygon, polyline,
    radialGradient, rect,
    script, set, stop, style, svg, switch, symbol,
    text, textPath, title, tref, tspan,
    use,
    view, vkern,
    ) where

import           Graphics.UI.Threepenny.Core (mkElement)


tag    = mkElement
itag   = mkElement

a                     =  tag "svg:a"
altGlyph              =  tag "svg:altGlyph"
altGlyphDef           = itag "svg:altGlyphDef"
altGlyphItem          = itag "svg:altGlyphItem"
animate               =  tag "svg:animate"
animateColor          =  tag "svg:animateColor"
animateMotion         =  tag "svg:animateMotion"
animateTransform      = itag "svg:animateTransform"
circle                = itag "svg:circle"
clipPath              =  tag "svg:clipPath"
colorProfile          = itag "svg:colorProfile"
cursor                = itag "svg:cursor"
defs                  =  tag "svg:defs"
desc                  = itag "svg:desc"
ellipse               =  tag "svg:ellipse"
feBlend               =  tag "svg:feBlend"
feColorMatrix         =  tag "svg:feColorMatrix"
feComponentTransfer   =  tag "svg:feComponentTransfer"
feComposite           =  tag "svg:feComposite"
feConvolveMatrix      =  tag "svg:feConvolveMatrix"
feDiffuseLighting     =  tag "svg:feDiffuseLighting"
feDisplacementMap     =  tag "svg:feDisplacementMap"
feDistantLight        = itag "svg:feDistantLight"
feFlood               =  tag "svg:feFlood"
feFuncA               = itag "svg:feFuncA"
feFuncB               = itag "svg:feFuncB"
feFuncG               = itag "svg:feFuncG"
feFuncR               = itag "svg:feFuncR"
feGaussianBlur        =  tag "svg:feGaussianBlur"
feImage               =  tag "svg:feImage"
feMerge               =  tag "svg:feMerge"
feMergeNode           = itag "svg:feMergeNode"
feMorphology          =  tag "svg:feMorphology"
feOffset              =  tag "svg:feOffset"
fePointLight          = itag "svg:fePointLight"
feSpecularLighting    =  tag "svg:feSpecularLighting"
feSpotLight           = itag "svg:feSpotLight"
feTile                =  tag "svg:feTile"
feTurbulence          =  tag "svg:feTurbulence"
filter_               =  tag "svg:filter"
font                  =  tag "svg:font"
fontface              = itag "svg:font-face"
fontfaceformat        = itag "svg:font-face-format"
fontfacename          = itag "svg:font-face-name"
fontfacesrv           = itag "svg:font-face-src"
fontfaceuri           = itag "svg:font-face-uri"
foreignObject         =  tag "svg:foreignObject"
g                     =  tag "svg:g"
glyph                 =  tag "svg:glyph"
glyphRef              =  tag "svg:glyphRef"
hkern                 = itag "svg:hkern"
image                 =  tag "svg:image"
line                  =  tag "svg:line"
linearGradient        =  tag "svg:linearGradient"
marker                =  tag "svg:marker"
mask                  =  tag "svg:mask"
metadata              = itag "svg:metadata"
missingglyph          =  tag "svg:missing-glyph"
mpath                 = itag "svg:mpath"
path                  = itag "svg:path"
pattern               =  tag "svg:pattern"
polygon               =  tag "svg:polygon"
polyline              =  tag "svg:polyline"
radialGradient        =  tag "svg:radialGradient"
rect                  =  tag "svg:rect"
script                = itag "svg:script"
set                   = itag "svg:set"
stop                  =  tag "svg:stop"
style                 = itag "svg:style"
svg                   =  tag "svg:svg"
switch                =  tag "svg:switch"
symbol                =  tag "svg:symbol"
text                  =  tag "svg:text"
textPath              =  tag "svg:textPath"
title                 = itag "svg:title"
tref                  =  tag "svg:tref"
tspan                 =  tag "svg:tspan"
use                   =  tag "svg:use"
view                  = itag "svg:view"
vkern                 = itag "svg:vkern"
