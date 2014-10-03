module Graphics.UI.Threepenny.SVG.Attributes (
    -- * Synopsis
    -- | SVG attributes as defined by W3C, Scalable Vector Graphics (SVG) 1.1
    --   (Second Edition) Appendix M <http://www.w3.org/TR/2011/REC-SVG11-20110816/>.
    -- 
    -- Whenever possible, the Haskell identifier for an attribute is the 
    -- same as the attribute name. However, sometimes changes are necessary:
    --
    --     1. Hyphens @-@ are replaced by underscores @_@, e.g. 'stroke_width'.
    --
    --     2. An underscore is used whenever the attribute name conflicts with 
    --       a reserved word in Haskell, e.g. 'class_'.
    
    -- * Regular attributes
    accent_height, accumulate, additive, alphabetic, amplitude,
    arabic_form, ascent, attributeName, attributeType, azimuth,
    baseFrequency, baseProfile, bbox, begin, bias, by,
    calcMode, cap_height, class_, clipPathUnits, contentScriptType,
    contentStyleType, cx, cy,
    d, descent, diffuseConstant, divisor, dur, dx, dy,
    edgeMode, elevation, end, exponent, externalResourcesRequired,
    filterRes, filterUnits, format, from, fx, fy,
    g1, g2, glyph_name, glyphRef, gradientTransform, gradientUnits,
    hanging, height, horiz_adv_x, horiz_origin_x, horiz_origin_y,
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
    overline_position, overline_thickness,
    panose_1, path, pathLength, patternContentUnits,
    patternTransform, patternUnits, points, pointsAtX, pointsAtY, pointsAtZ,
    preserveAlpha, preserveAspectRatio, primitiveUnits,
    r, radius, refx, refy, rendering_intent, repeatCount, repeatDur,
    requiredExtensions, requiredFeatures, restart, result, rotate, rx, ry,
    scale, seed, slope, spacing, specularConstant, specularExponent, spreadMethod,
    startOffset, stdDeviation, stemh, stemv, stitchTiles,
    strikethrough_position, strikethrough_thickness,
    string, style, surfaceScale, systemLanguage,
    tableValues, target, targetX, targetY, textLength, title, to, transform, type_,
    u1, u2, underline_position, underline_thickness, unicode, unicode_range, units_per_em,
    v_alphabetic, v_hanging, v_ideographic, v_mathematical, values, version,
    vert_adv_y, vert_origin_x, vert_origin_y, viewBox, viewTarget,
    width, widths,
    x, x_height, x1, x2, xChannelSelector,
    xlink_actuate, xlink_arcrole, xlink_href, xlink_role, xlink_show, xlink_title, xlink_type,
    xml_base, xml_lang, xml_space,
    y, y1, y2, yChannelSelector,
    z, zoomAndPan,

    -- * Presentation attributes
    alignment_baseline, baseline_shift,
    clip_path, clip_rule, clip, color_interpolation_filters, color_interpolation,
    color_profile, color_rendering, color, cursor,
    direction, display, dominant_baseline,
    enable_background,
    fill_opacity, fill_rule, fill, filter, flood_color,flood_opacity,
    font_family, font_size_adjust, font_size, font_stretch, font_style, font_variant,
    font_weight,
    glyph_orientation_horizontal, glyph_orientation_vertical,
    image_rendering, kerning,
    letter_spacing, lighting_color,
    marker_end, marker_mid, marker_start, mask,
    opacity, overflow, pointer_events,
    shape_rendering, stop_color, stop_opacity,
    stroke_dasharray, stroke_dashoffset, stroke_linecap, stroke_linejoin,
    stroke_miterlimit, stroke_opacity, stroke_width, stroke,
    text_anchor, text_decoration, text_rendering,
    unicode_bidi, visibility, word_spacing, writing_mode
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

accent_height                =  fltAttr "accent-height"
accumulate                   =  strAttr "accumulate"
additive                     =  strAttr "additive"
alignment_baseline           =  strAttr "alignment-baseline"
alphabetic                   =  fltAttr "alphabetic"
amplitude                    =  fltAttr "amplitude"
arabic_form                  =  strAttr "arabic-form"
ascent                       =  fltAttr "ascent"
attributeName                =  strAttr "attributeName"
attributeType                =  strAttr "attributeType"
azimuth                      =  fltAttr "azimuth"
baseFrequency                =  strAttr "baseFrequency"
baseProfile                  =  strAttr "baseProfile"
baseline_shift               =  strAttr "baseline-shift"
bbox                         =  strAttr "bbox"
begin                        =  strAttr "begin"
bias                         =  fltAttr "bias"
by                           =  fltAttr "by"
calcMode                     =  strAttr "calcMode"
cap_height                   =  fltAttr "cap-height"
class_                       =  strAttr "class"
clip                         =  strAttr "clip"
clip_path                    =  strAttr "clip-path"
clip_rule                    =  strAttr "clip-rule"
clipPathUnits                =  strAttr "clipPathUnits"
color                        =  strAttr "color"
color_interpolation          =  strAttr "color-interpolation"
color_interpolation_filters  =  strAttr "color-interpolation-filters"
color_profile                =  strAttr "color-profile"
color_rendering              =  strAttr "color-rendering"
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
dominant_baseline            =  strAttr "dominant-baseline"
dur                          =  strAttr "dur"
dx                           =  strAttr "dx"
dy                           =  strAttr "dy"
edgeMode                     =  strAttr "edgeMode"
elevation                    =  fltAttr "elevation"
enable_background            =  strAttr "enable-background"
end                          =  strAttr "end"
exponent                     =  fltAttr "exponent"
externalResourcesRequired    =  strAttr "externalResourcesRequired"
fill                         =  strAttr "fill"
fill_opacity                 =  strAttr "fill-opacity"
fill_rule                    =  strAttr "fill-rule"
filter                       =  strAttr "filter"
filterRes                    =  strAttr "filterRes"
filterUnits                  =  strAttr "filterUnits"
flood_color                  =  strAttr "flood-color"
flood_opacity                =  strAttr "flood-opacity"
font_family                  =  strAttr "font-family"
font_size                    =  strAttr "font-size"
font_size_adjust             =  strAttr "font-size-adjust"
font_stretch                 =  strAttr "font-stretch"
font_style                   =  strAttr "font-style"
font_variant                 =  strAttr "font-variant"
font_weight                  =  strAttr "font-weight"
format                       =  strAttr "format"
from                         =  fltAttr "from"
fx                           =  strAttr "fx"
fy                           =  strAttr "fy"
g1                           =  strAttr "g1"
g2                           =  strAttr "g2"
glyph_name                   =  strAttr "glyph-name"
glyph_orientation_horizontal =  strAttr "glyph-orientation-horizontal"
glyph_orientation_vertical   =  strAttr "glyph-orientation-vertical"
glyphRef                     =  strAttr "glyphRef"
gradientTransform            =  strAttr "gradientTransform"
gradientUnits                =  strAttr "gradientUnits"
hanging                      =  fltAttr "hanging"
height                       =  strAttr "height"
horiz_adv_x                  =  fltAttr "horiz-adv-x"
horiz_origin_x               =  fltAttr "horiz-origin-x"
horiz_origin_y               =  fltAttr "horiz-origin-y"
id                           =  strAttr "id"
ideographic                  =  fltAttr "ideographic"
image_rendering              =  strAttr "image-rendering"
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
letter_spacing               =  strAttr "letter-spacing"
lighting_color               =  strAttr "lighting-color"
limitingConeAngle            =  fltAttr "limitingConeAngle"
local                        =  strAttr "local"
marker_end                   =  strAttr "marker-end"
marker_mid                   =  strAttr "marker-mid"
marker_start                 =  strAttr "marker-start"
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
overline_position            =  fltAttr "overline-position"
overline_thickness           =  fltAttr "overline-thickness"
panose_1                     =  intAttr "panose-1"
path                         =  "path"
-- path                         =  strAttr "path"
pathLength                   =  fltAttr "pathLength"
patternContentUnits          =  strAttr "patternContentUnits"
patternTransform             =  strAttr "patternTransform"
patternUnits                 =  strAttr "patternUnits"
pointer_events               =  strAttr "pointer-events"
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
rendering_intent             =  strAttr "rendering-intent"
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
shape_rendering              =  strAttr "shape-rendering"
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
stop_color                   =  strAttr "stop-color"
stop_opacity                 =  strAttr "stop-opacity"
strikethrough_position       =  fltAttr "strikethrough-position"
strikethrough_thickness      =  fltAttr "strikethrough-thickness"
string                       =  strAttr "string"
stroke                       =  strAttr "stroke"
stroke_dasharray             =  strAttr "stroke-dasharray"
stroke_dashoffset            =  strAttr "stroke-dashoffset"
stroke_linecap               =  strAttr "stroke-linecap"
stroke_linejoin              =  strAttr "stroke-linejoin"
stroke_miterlimit            =  strAttr "stroke-miterlimit"
stroke_opacity               =  strAttr "stroke-opacity"
stroke_width                 =  strAttr "stroke-width"
style                        =  strAttr "style"
surfaceScale                 =  fltAttr "surfaceScale"
systemLanguage               =  strAttr "systemLanguage"
tableValues                  =  strAttr "tableValues"
target                       =  strAttr "target"
targetX                      =  fltAttr "targetX"
targetY                      =  fltAttr "targetY"
text_anchor                  =  strAttr "text-anchor"
text_decoration              =  strAttr "text-decoration"
text_rendering               =  strAttr "text-rendering"
textLength                   =  strAttr "textLength"
title                        =  strAttr "title"
to                           =  fltAttr "to"
transform                    =  strAttr "transform"
type_                        =  strAttr "type"
u1                           =  strAttr "u1"
u2                           =  strAttr "u2"
underline_position           =  fltAttr "underline-position"
underline_thickness          =  fltAttr "underline-thickness"
unicode                      =  strAttr "unicode"
unicode_bidi                 =  strAttr "unicode-bidi"
unicode_range                =  strAttr "unicode-range"
units_per_em                 =  fltAttr "units-per-em"
v_alphabetic                 =  fltAttr "v-alphabetic"
v_hanging                    =  fltAttr "v-hanging"
v_ideographic                =  fltAttr "v-ideographic"
v_mathematical               =  fltAttr "v-mathematical"
values                       =  strAttr "values"
version                      =  fltAttr "version"
vert_adv_y                   =  fltAttr "vert-adv-y"
vert_origin_x                =  fltAttr "vert-origin-x"
vert_origin_y                =  fltAttr "vert-origin-y"
viewBox                      =  strAttr "viewBox"
viewTarget                   =  strAttr "viewTarget"
visibility                   =  strAttr "visibility"
width                        =  strAttr "width"
widths                       =  strAttr "widths"
word_spacing                 =  strAttr "word-spacing"
writing_mode                 =  strAttr "writing-mode"
x                            =  strAttr "x"
x_height                     =  fltAttr "x-height"
x1                           =  strAttr "x1"
x2                           =  strAttr "x2"
xChannelSelector             =  strAttr "xChannelSelector"
xlink_actuate                =  strAttr "xlink:actuate"
xlink_arcrole                =  strAttr "xlink:acrole"
xlink_href                   =  strAttr "xlink:href"
xlink_role                   =  strAttr "xlink:role"
xlink_show                   =  strAttr "xlink:show"
xlink_title                  =  strAttr "xlink:title"
xlink_type                   =  strAttr "xlink:type"
xml_base                     =  strAttr "xml:base"
xml_lang                     =  strAttr "xml:lang"
xml_space                    =  strAttr "xml:space"
y                            =  strAttr "y"
y1                           =  strAttr "y1"
y2                           =  strAttr "y2"
yChannelSelector             =  strAttr "yChannelSelector"
z                            =  fltAttr "z"
zoomAndPan                   =  strAttr "zoomAndPan"
