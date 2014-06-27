--------------------------------------------------------------------------------
-- |
-- Module      : Graphics.UI.Threepenny.ElementsSVG
-- Description : Elements used in SVG markup
--
--  Elements are defined by W3C, Scalable Vector Graphics (SVG) 1.1
--    (Second Edition) Appendix M <http://www.w3.org/TR/2011/REC-SVG11-20110816/>.
--
--  The prefix 'svg_' is utilized on those exposed elements that conflict with
--  existing threepenny-gui symbols or have analogs defined in the HTML namespace.
--
--------------------------------------------------------------------------------

module Graphics.UI.Threepenny.ElementsSVG (
    -- * SVG Elements
    svg_a, altGlyph, altGlyphDef, altGlyphItem,
    svg_animate, animateColor, animateMotion, animateTransform,
    circle, clipPath, color_profile, cursor,
    defs, desc,
    ellipse,
    feBlend, feColorMatrix, feComponentTransfer, feComposite, feConvolveMatrix,
    feDiffuseLighting, feDisplacementMap, feDistantLight, feFlood,
    feFuncA, feFuncB, feFuncG, feFuncR,
    feGaussianBlur, feImage, feMerge, feMergeNode, feMorphology, feOffset,
    fePointLight, feSpecularLighting, feSpotLight, feTile, feTurbulence,
    svg_filter, svg_font, font_face, font_face_format, font_face_name,
    font_face_srv, font_face_uri, foreignObject,
    g, glyph, glyphRef,
    hkern,
    svg_image,
    line, linearGradient,
    marker, mask, metadata, missing_glyph, mpath,
    path, pattern, polygon, polyline,
    radialGradient, rect,
    script, svg_set, svg_stop, svg_style, svg, switch, symbol,
    svg_text, textPath, svg_title, tref, tspan,
    use,
    view, vkern,
    ) where

import           Graphics.UI.Threepenny.Core (mkElement)


tag    = mkElement
itag   = mkElement

svg_a                 =  tag "svg:a"
altGlyph              =  tag "svg:altGlyph"
altGlyphDef           = itag "svg:altGlyphDef"
altGlyphItem          = itag "svg:altGlyphItem"
svg_animate           =  tag "svg:animate"
animateColor          =  tag "svg:animateColor"
animateMotion         =  tag "svg:animateMotion"
animateTransform      = itag "svg:animateTransform"
circle                = itag "svg:circle"
clipPath              =  tag "svg:clipPath"
color_profile         = itag "svg:color-profile"
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
svg_filter            =  tag "svg:filter"
svg_font              =  tag "svg:font"
font_face             = itag "svg:font-face"
font_face_format      = itag "svg:font-face-format"
font_face_name        = itag "svg:font-face-name"
font_face_srv         = itag "svg:font-face-src"
font_face_uri         = itag "svg:font-face-uri"
foreignObject         =  tag "svg:foreignObject"
g                     =  tag "svg:g"
glyph                 =  tag "svg:glyph"
glyphRef              =  tag "svg:glyphRef"
hkern                 = itag "svg:hkern"
svg_image             =  tag "svg:image"
line                  =  tag "svg:line"
linearGradient        =  tag "svg:linearGradient"
marker                =  tag "svg:marker"
mask                  =  tag "svg:mask"
metadata              = itag "svg:metadata"
missing_glyph         =  tag "svg:missing-glyph"
mpath                 = itag "svg:mpath"
path                  = itag "svg:path"
pattern               =  tag "svg:pattern"
polygon               =  tag "svg:polygon"
polyline              =  tag "svg:polyline"
radialGradient        =  tag "svg:radialGradient"
rect                  =  tag "svg:rect"
script                = itag "svg:script"
svg_set               = itag "svg:set"
svg_stop              =  tag "svg:stop"
svg_style             = itag "svg:style"
svg                   =  tag "svg:svg"
switch                =  tag "svg:switch"
symbol                =  tag "svg:symbol"
svg_text              =  tag "svg:text"
textPath              =  tag "svg:textPath"
svg_title             = itag "svg:title"
tref                  =  tag "svg:tref"
tspan                 =  tag "svg:tspan"
use                   =  tag "svg:use"
view                  = itag "svg:view"
vkern                 = itag "svg:vkern"
