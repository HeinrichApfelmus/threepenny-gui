--------------------------------------------------------------------------------
-- |
-- Module      : Graphics.UI.Threepenny.SVG.Attributes
-- Description : Attributes used in SVG markup
--
--  Attributes are defined by W3C, Scalable Vector Graphics (SVG) 1.1
--    (Second Edition) Appendix M <http://www.w3.org/TR/2011/REC-SVG11-20110816/>.
--
--  The suffix '_' is utilized on those exposed attributes that conflict with
--  Haskell reserved words.
--
--------------------------------------------------------------------------------
module Graphics.UI.Threepenny.SVG.Attributes (
    -- * Synopsis
    -- | Regular attributes.
    accentheight, accumulate, additive, alphabetic, amplitude,
    arabicform, ascent, attributeName, attributeType, azimuth,
    baseFrequency, baseProfile, bbox, begin, bias, by,
    calcMode, capheight, class_, clipPathUnits, contentScriptType,
    contentStyleType, cx, cy,
    d, descent, diffuseConstant, divisor, dur, dx, dy,
    edgeMode, elevation, end, exponent, externalResourcesRequired,
    filterRes, filterUnits, format, from, fx, fy,
    g1, g2, glyphname, glyphRef, gradientTransform, gradientUnits,
    hanging, height, horizadvx, horizoriginx, horizoriginy,
    id, ideographic, in_, in2, intercept,
    k, k1, k2, k3, k4, kernelMatrix, kernelUnitLength,
    keyPoints, keySplines, keyTimes,
    lang, lengthAdjust, limitingConeAngle, local,
    markerHeight, markerUnits, markerWidth, maskContentUnits, maskUnits,
    mathematical, max, media, method, min, mode,
    name, numOctaves,
    offset,
    onabort, onactivate, onbegin, onclick, onend, onerror, onfocusin, onfocusout,
    onload, onmousedown, onmousemove, onmouseout, onmouseover, onmouseup,
    onrepeat, onresize, onscroll, onunload, onzoom,
    operator, order, orient, orientation, origin,
    overlineposition, overlinethickness,
    panose1, path, pathLength, patternContentUnits,
    patternTransform, patternUnits, points, pointsAtX, pointsAtY, pointsAtZ,
    preserveAlpha, preserveAspectRatio, primitiveUnits,
    r, radius, refx, refy, renderingintent, repeatCount, repeatDur,
    requiredExtensions, requiredFeatures, restart, result, rotate, rx, ry,
    scale, seed, slope, spacing, specularConstant, specularExponent, spreadMethod,
    startOffset, stdDeviation, stemh, stemv, stitchTiles,
    strikethroughposition, strikethroughthickness,
    string, style, surfaceScale, systemLanguage,
    tableValues, target, targetX, targetY, textLength, title, to, transform, type_,
    u1, u2, underlineposition, underlinethickness, unicode, unicoderange, unitsperem,
    valphabetic, vhanging, videographic, vmathematical, values, version,
    vertadvy, vertoriginx, vertoriginy, viewBox, viewTarget,
    width, widths,
    x, xheight, x1, x2, xChannelSelector,
    xlinkactuate, xlinkarcrole, xlinkhref, xlinkrole, xlinkshow, xlinktitle, xlinktype,
    xmlbase, xmllang, xmlspace,
    y, y1, y2, yChannelSelector,
    z, zoomAndPan,

    -- | Presentation Attributres
    alignmentbaseline, baselineshift,
    clippath, cliprule, clip, colorinterpolationfilters, colorinterpolation,
    colorprofile, colorrendering, color, cursor,
    direction, display, dominantbaseline,
    enablebackground,
    fillopacity,fillrule, fill, filter, floodcolor,floodopacity,
    fontfamily,fontsizeadjust, fontsize, fontstretch, fontstyle, fontvariant,
    fontweight,
    glyphorientationhorizontal, glyphorientationvertical,
    imagerendering,kerning,
    letterspacing,lightingcolor,
    markerend,markermid, markerstart, mask,
    opacity, overflow, pointerevents,
    shaperendering,stopcolor, stopopacity,
    strokedasharray,strokedashoffset, strokelinecap, strokelinejoin,
    strokemiterlimit,strokeopacity, strokewidth, stroke,
    textanchor,textdecoration, textrendering,
    unicodebidi,visibility, wordspacing, writingmode
    ) where

import           Graphics.UI.Threepenny.Core (Element, WriteAttr, attr,
                                              mkWriteAttr, set')
import           Prelude                     hiding (exponent, filter, id, max,
                                              min)

strAttr :: String -> WriteAttr Element String
strAttr n = mkWriteAttr (set' (attr n))

intAttr :: String -> WriteAttr Element Int
intAttr n = mkWriteAttr (set' (attr n) . show)

fltAttr :: String -> WriteAttr Element Float
fltAttr n = mkWriteAttr (set' (attr n) . show)

accentheight                 =  fltAttr "accent-height"
accumulate                   =  strAttr "accumulate"
additive                     =  strAttr "additive"
alignmentbaseline            =  strAttr "alignment-baseline"
alphabetic                   =  fltAttr "alphabetic"
amplitude                    =  fltAttr "amplitude"
arabicform                   =  strAttr "arabic-form"
ascent                       =  fltAttr "ascent"
attributeName                =  strAttr "attributeName"
attributeType                =  strAttr "attributeType"
azimuth                      =  fltAttr "azimuth"
baseFrequency                =  strAttr "baseFrequency"
baseProfile                  =  strAttr "baseProfile"
baselineshift                =  strAttr "baseline-shift"
bbox                         =  strAttr "bbox"
begin                        =  strAttr "begin"
bias                         =  fltAttr "bias"
by                           =  fltAttr "by"
calcMode                     =  strAttr "calcMode"
capheight                    =  fltAttr "cap-height"
class_                       =  strAttr "class"
clip                         =  strAttr "clip"
clippath                     =  strAttr "clip-path"
cliprule                     =  strAttr "clip-rule"
clipPathUnits                =  strAttr "clipPathUnits"
color                        =  strAttr "color"
colorinterpolation           =  strAttr "color-interpolation"
colorinterpolationfilters    =  strAttr "color-interpolation-filters"
colorprofile                 =  strAttr "color-profile"
colorrendering               =  strAttr "color-rendering"
contentScriptType            =  strAttr "contentScriptType"
contentStyleType             =  strAttr "contentStyleType"
cursor                       =  strAttr "cursor"
cx                           =  strAttr "cx"
cy                           =  strAttr "cy"
d                            =  strAttr "d"
descent                      =  fltAttr "descent"
diffuseConstant              =  fltAttr "diffuseConstant"
direction                    =  strAttr "direction"
display                      =  strAttr "display"
divisor                      =  fltAttr "divisor"
dominantbaseline             =  strAttr "dominant-baseline"
dur                          =  strAttr "dur"
dx                           =  strAttr "dx"
dy                           =  strAttr "dy"
edgeMode                     =  strAttr "edgeMode"
elevation                    =  fltAttr "elevation"
enablebackground             =  strAttr "enable-background"
end                          =  strAttr "end"
exponent                     =  fltAttr "exponent"
externalResourcesRequired    =  strAttr "externalresourcesrequired"
fill                         =  strAttr "fill"
fillopacity                  =  strAttr "fill-opacity"
fillrule                     =  strAttr "fill-rule"
filter                     =  strAttr "filter"
filterRes                    =  strAttr "filterRes"
filterUnits                  =  strAttr "filterUnits"
floodcolor                   =  strAttr "flood-color"
floodopacity                 =  strAttr "flood-opacity"
fontfamily                   =  strAttr "font-family"
fontsize                     =  strAttr "font-size"
fontsizeadjust               =  strAttr "font-size-adjust"
fontstretch                  =  strAttr "font-stretch"
fontstyle                    =  strAttr "font-style"
fontvariant                  =  strAttr "font-variant"
fontweight                   =  strAttr "font-weight"
format                       =  strAttr "format"
from                         =  fltAttr "from"
fx                           =  strAttr "fx"
fy                           =  strAttr "fy"
g1                           =  strAttr "g1"
g2                           =  strAttr "g2"
glyphname                    =  strAttr "glyph-name"
glyphorientationhorizontal   =  strAttr "glyph-orientation-horizontal"
glyphorientationvertical     =  strAttr "glyph-orientation-vertical"
glyphRef                     =  strAttr "glyphRef"
gradientTransform            =  strAttr "gradientTransform"
gradientUnits                =  strAttr "gradientUnits"
hanging                      =  fltAttr "hanging"
height                       =  strAttr "height"
horizadvx                    =  fltAttr "horiz-adv-x"
horizoriginx                 =  fltAttr "horiz-origin-x"
horizoriginy                 =  fltAttr "horiz-origin-y"
id                           =  strAttr "id"
ideographic                  =  fltAttr "ideographic"
imagerendering               =  strAttr "image-rendering"
in_                          =  strAttr "in"
in2                          =  strAttr "in2"
intercept                    =  fltAttr "intercept"
k                            =  fltAttr "k"
k1                           =  fltAttr "k1"
k2                           =  fltAttr "k2"
k3                           =  fltAttr "k3"
k4                           =  fltAttr "k4"
kernelMatrix                 =  strAttr "kernelMatrix"
kernelUnitLength             =  strAttr "kernelUnitLength"
kerning                      =  strAttr "kerning"
keyPoints                    =  strAttr "keyPoints"
keySplines                   =  strAttr "keySplines"
keyTimes                     =  strAttr "keyTimes"
lang                         =  strAttr "lang"
lengthAdjust                 =  strAttr "lengthAdjust"
letterspacing                =  strAttr "letter-spacing"
lightingcolor                =  strAttr "lighting-color"
limitingConeAngle            =  fltAttr "limitingConeAngle"
local                        =  strAttr "local"
markerend                    =  strAttr "marker-end"
markermid                    =  strAttr "marker-mid"
markerstart                  =  strAttr "marker-start"
markerHeight                 =  strAttr "markerHeight"
markerUnits                  =  strAttr "markerUnits"
markerWidth                  =  strAttr "markerWidth"
mask                         =  strAttr "mask"
maskContentUnits             =  strAttr "maskContentUnits"
maskUnits                    =  strAttr "maskUnits"
mathematical                 =  fltAttr "mathematical"
max                          =  strAttr "max"
media                        =  strAttr "media"
method                       =  strAttr "method"
min                          =  strAttr "min"
mode                         =  strAttr "mode"
name                         =  strAttr "name"
numOctaves                   =  intAttr "numOctaves"
offset                       =  fltAttr "offset"
onabort                      =  strAttr "onabort"
onactivate                   =  strAttr "onactivate"
onbegin                      =  strAttr "onbegin"
onclick                      =  strAttr "onclick"
onend                        =  strAttr "onend"
onerror                      =  strAttr "onerror"
onfocusin                    =  strAttr "onfocusin"
onfocusout                   =  strAttr "onfocusout"
onload                       =  strAttr "onload"
onmousedown                  =  strAttr "onmousedown"
onmousemove                  =  strAttr "onmousemove"
onmouseout                   =  strAttr "onmouseout"
onmouseover                  =  strAttr "onmouseover"
onmouseup                    =  strAttr "onmouseup"
onrepeat                     =  strAttr "onrepeat"
onresize                     =  strAttr "onresize"
onscroll                     =  strAttr "onscroll"
onunload                     =  strAttr "onunload"
onzoom                       =  strAttr "onzoom"
opacity                      =  strAttr "opacity"
operator                     =  strAttr "operator"
order                        =  strAttr "order"
orient                       =  strAttr "orient"
orientation                  =  strAttr "orientation"
origin                       =  strAttr "origin"
overflow                     =  strAttr "overflow"
overlineposition             =  fltAttr "overline-position"
overlinethickness            =  fltAttr "overline-thickness"
panose1                      =   intAttr "panose-1"
path                         =  "path"
-- path                         =  strAttr "path"
pathLength                   =  fltAttr "pathLength"
patternContentUnits          =  strAttr "patternContentUnits"
patternTransform             =  strAttr "patternTransform"
patternUnits                 =  strAttr "patternUnits"
pointerevents                =  strAttr "pointer-events"
points                       =  strAttr "points"
pointsAtX                    =  fltAttr "pointsAtX"
pointsAtY                    =  fltAttr "pointsAtY"
pointsAtZ                    =  fltAttr "pointsAtZ"
preserveAlpha                =  strAttr "preserveAlpha"
preserveAspectRatio          =  strAttr "preserveAspectRatio"
primitiveUnits               =  strAttr "primitiveUnits"
r                            =  strAttr "r"
radius                       =  strAttr "radius"
refx                         =  strAttr "refx"
refy                         =  strAttr "refy"
renderingintent              =  strAttr "rendering-intent"
repeatCount                  =  strAttr "repeatCount"
repeatDur                    =  strAttr "repeatDur"
requiredExtensions           =  strAttr "requiredExtensions"
requiredFeatures             =  strAttr "requiredFeatures"
restart                      =  strAttr "restart"
result                       =  strAttr "result"
rotate                       =  strAttr "rotate"
rx                           =  strAttr "rx"
ry                           =  strAttr "ry"
scale                        =  fltAttr "scale"
seed                         =  fltAttr "seed"
shaperendering               =  strAttr "shape-rendering"
slope                        =  fltAttr "slope"
spacing                      =  strAttr "spacing"
specularConstant             =  fltAttr "specularConstant"
specularExponent             =  fltAttr "specularExponent"
spreadMethod                 =  strAttr "spreadMethod"
startOffset                  =  strAttr "startOffset"
stdDeviation                 =  strAttr "stdDeviation"
stemh                        =  fltAttr "stemh"
stemv                        =  fltAttr "stemv"
stitchTiles                  =  strAttr "stitchTiles"
stopcolor                    =  strAttr "stop-color"
stopopacity                  =  strAttr "stop-opacity"
strikethroughposition        =  fltAttr "strikethrough-position"
strikethroughthickness       =  fltAttr "strikethrough-thickness"
string                       =  strAttr "string"
stroke                       =  strAttr "stroke"
strokedasharray              =  strAttr "stroke-dasharray"
strokedashoffset             =  strAttr "stroke-dashoffset"
strokelinecap                =  strAttr "stroke-linecap"
strokelinejoin               =  strAttr "stroke-linejoin"
strokemiterlimit             =  strAttr "stroke-miterlimit"
strokeopacity                =  strAttr "stroke-opacity"
strokewidth                  =  strAttr "stroke-width"
style                        =  strAttr "style"
surfaceScale                 =  fltAttr "surfaceScale"
systemLanguage               =  strAttr "systemLanguage"
tableValues                  =  strAttr "tableValues"
target                       =  strAttr "target"
targetX                      =  fltAttr "targetX"
targetY                      =  fltAttr "targetY"
textanchor                   =  strAttr "text-anchor"
textdecoration               =  strAttr "text-decoration"
textrendering                =  strAttr "text-rendering"
textLength                   =  strAttr "textLength"
title                        =  strAttr "title"
to                           =  fltAttr "to"
transform                    =  strAttr "transform"
type_                        =  strAttr "type"
u1                           =  strAttr "u1"
u2                           =  strAttr "u2"
underlineposition            =  fltAttr "underline-position"
underlinethickness           =  fltAttr "underline-thickness"
unicode                      =  strAttr "unicode"
unicodebidi                  =  strAttr "unicode-bidi"
unicoderange                 =  strAttr "unicode-range"
unitsperem                   =  fltAttr "units-per-em"
valphabetic                  =  fltAttr "v-alphabetic"
vhanging                     =  fltAttr "v-hanging"
videographic                 =  fltAttr "v-ideographic"
vmathematical                =  fltAttr "v-mathematical"
values                       =  strAttr "values"
version                      =  fltAttr "version"
vertadvy                     =  fltAttr "vert-adv-y"
vertoriginx                  =  fltAttr "vert-origin-x"
vertoriginy                  =  fltAttr "vert-origin-y"
viewBox                      =  strAttr "viewBox"
viewTarget                   =  strAttr "viewTarget"
visibility                   =  strAttr "visibility"
width                        =  strAttr "width"
widths                       =  strAttr "widths"
wordspacing                  =  strAttr "word-spacing"
writingmode                  =  strAttr "writing-mode"
x                            =  strAttr "x"
xheight                      =  fltAttr "x-height"
x1                           =  strAttr "x1"
x2                           =  strAttr "x2"
xChannelSelector             =  strAttr "xChannelSelector"
xlinkactuate                 =  strAttr "xlink:actuate"
xlinkarcrole                 =  strAttr "xlink:acrole"
xlinkhref                    =  strAttr "xlink:href"
xlinkrole                    =  strAttr "xlink:role"
xlinkshow                    =  strAttr "xlink:show"
xlinktitle                   =  strAttr "xlink:title"
xlinktype                    =  strAttr "xlink:type"
xmlbase                      =  strAttr "xml:base"
xmllang                      =  strAttr "xml:lang"
xmlspace                     =  strAttr "xml:Space"
y                            =  strAttr "y"
y1                           =  strAttr "y1"
y2                           =  strAttr "y2"
yChannelSelector             =  strAttr "yChannelSelector"
z                            =  fltAttr "z"
zoomAndPan                   =  strAttr "zoomAndPan"
