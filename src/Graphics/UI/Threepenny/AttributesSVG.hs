--------------------------------------------------------------------------------
-- |
-- Module      : Graphics.UI.Threepenny.AttributesSVG
-- Description : Attributes used in SVG markup
--
--  Attributes are defined by W3C, Scalable Vector Graphics (SVG) 1.1
--    (Second Edition) Appendix M <http://www.w3.org/TR/2011/REC-SVG11-20110816/>.
--
--  The prefix 'svg_' is utilized on those exposed attributes that conflict with
--  existing threepenny-gui symbols or have analogs defined in the HTML namespace.
--  The suffix '_' is utilized on those exposed attributes that conflict with
--  symbols exposed as SVG Elements.
--
--------------------------------------------------------------------------------
module Graphics.UI.Threepenny.AttributesSVG (
    -- * Synopsis
    -- | Regular attributes.
    accent_height, accumulate, additive, alphabetic, amplitude,
    arabic_form, ascent, attributeName, attributeType, azimuth,
    baseFrequency, baseProfile, bbox, begin, bias, by,
    calcMode, cap_height, svg_class, clipPathUnits, contentScriptType,
    contentStyleType, cx, cy,
    d, descent, diffuseConstant, divisor, dur, dx, dy,
    edgeMode, elevation, end, svg_exponent, externalResourcesRequired,
    filterRes, filterUnits, format, from, fx, fy,
    g1, g2, glyph_name, glyphRef_, gradientTransform, gradientUnits,
    hanging, svg_height, horiz_adv_x, horiz_origin_x, horiz_origin_y,
    svg_id, ideographic, svg_in, in2, intercept,
    k, k1, k2, k3, k4, kernelMatrix, kernelUnitLength,
    keyPoints, keySplines, keyTimes,
    svg_lang, lengthAdjust, limitingConeAngle, local,
    markerHeight, markerUnits, markerWidth, maskContentUnits, maskUnits,
    mathematical, svg_max, media, svg_method, svg_min, mode,
    svg_name, numOctaves,
    offset,
    onabort, onactivate, onbegin, onclick, onend, onerror, onfocusin, onfocusout,
    onload, onmousedown, onmousemove, onmouseout, onmouseover, onmouseup,
    onrepeat, onresize, onscroll, onunload, onzoom,
    operator, order, orient, orientation, origin,
    overline_position, overline_thickness,
    panose_1, path_, pathLength, patternContentUnits,
    patternTransform, patternUnits, points, pointsAtX, pointsAtY, pointsAtZ,
    preserveAlpha, preserveAspectRatio, primitiveUnits,
    r, radius, refx, refy, rendering_intent, repeatCount, repeatDur,
    requiredExtensions, requiredFeatures, restart, result, rotate, rx, ry,
    scale, seed, slope, spacing, specularConstant, specularExponent, spreadMethod,
    startOffset, stdDeviation, stemh, stemv, stitchTiles, strikethrough_position,
    strikethrough_thickness, svg_string, svg_style_, surfaceScale, systemLanguage,
    tableValues, svg_target, targetX, targetY, textLength, svg_title_,
    to, transform, svg_type,
    u1, u2, underline_position, underline_thickness,
    unicode, unicode_range, units_per_em,
    v_alphabetic, v_hanging, v_ideographic, v_mathematical, values, svg_version,
    vert_adv_y, vert_origin_x, vert_origin_y, viewBox, viewTarget,
    svg_width, widths,
    x, x_height, x1, x2, xChannelSelector,
    xlink_actuate, xlink_arcrole, xlink_href, xlink_role, xlink_show,
    xlink_title, xlink_type,
    xml_base, xml_lang, xml_space,
    y, y1, y2, yChannelSelector,
    z, zoomAndPan,

    -- * Presentation Attributres
    alignment_baseline, baseline_shift,
    clip_path, clip_rule, clip,
    color_interpolation_filters, color_interpolation, color_profile_,
    color_rendering, svg_color, cursor_,
    direction, display, dominant_baseline, enable_background,
    fill_opacity, fill_rule, fill, filter, flood_color, flood_opacity,
    font_family, font_size_adjust, font_size, font_stretch, font_style,
    font_variant, font_weight,
    glyph_orientation_horizontal, glyph_orientation_vertical,
    image_rendering, kerning,
    letter_spacing, lighting_color,
    marker_end, marker_mid, marker_start, mask_,
    opacity, overflow, pointer_events,
    shape_rendering, stop_color, stop_opacity,
    stroke_dasharray, stroke_dashoffset, stroke_linecap, stroke_linejoin,
    stroke_miterlimit, stroke_opacity, stroke_width, stroke,
    text_anchor, text_decoration, text_rendering,
    unicode_bidi, visibility, word_spacing, writing_mode
    ) where

import           Graphics.UI.Threepenny.Core

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
baseline_shift               =  strAttr "baseline_shift"
bbox                         =  strAttr "bbox"
begin                        =  strAttr "begin"
bias                         =  fltAttr "bias"
by                           =  fltAttr "by"
calcMode                     =  strAttr "calcMode"
cap_height                   =  fltAttr "cap-height"
svg_class                    =  strAttr "class"
clip                         =  strAttr "clip"
clip_path                    =  strAttr "clip_path"
clip_rule                    =  strAttr "clip_rule"
clipPathUnits                =  strAttr "clipPathUnits"
svg_color                    =  strAttr "color"
color_interpolation          =  strAttr "color-interpolation"
color_interpolation_filters  =  strAttr "color-interpolation-filters"
color_profile_               =  strAttr "color-profile"
color_rendering              =  strAttr "color-rendering"
contentScriptType            =  strAttr "contentScriptType"
contentStyleType             =  strAttr "contentStyleType"
cursor_                      =  strAttr "cursor"
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
svg_exponent                 =  fltAttr "exponent"
externalResourcesRequired    =  strAttr "externalresourcesrequired"
fill                         =  strAttr "fill"
fill_opacity                 =  strAttr "fill-opacity"
fill_rule                    =  strAttr "fill-rule"
svg_filter                   =  strAttr "filter"
filterRes                    =  strAttr "filterRes"
filterUnits                  =  strAttr "filterUnits"
flood_color                  =  strAttr "flood-color"
flood_opacity                =  strAttr "flood-opacity"
font_family                  =  strAttr "font-Family"
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
glyph_orientation_vertical   =  strAttr "glyph-orientation_vertical"
glyphRef_                    =  strAttr "glyphRef"
gradientTransform            =  strAttr "gradientTransform"
gradientUnits                =  strAttr "gradientUnits"
hanging                      =  fltAttr "hanging"
svg_height                   =  strAttr "height"
horiz_adv_x                  =  fltAttr "horiz-adv-x"
horiz_origin_x               =  fltAttr "horiz-origin-x"
horiz_origin_y               =  fltAttr "horiz-origin-y"
svg_id                       =  strAttr "id"
ideographic                  =  fltAttr "ideographic"
image_rendering              =  strAttr "image-rendering"
svg_in                       =  strAttr "in"
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
svg_lang                     =  strAttr "lang"
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
mask_                        =  strAttr "mask"
maskContentUnits             =  strAttr "maskContentUnits"
maskUnits                    =  strAttr "maskUnits"
mathematical                 =  fltAttr "mathematical"
svg_max                      =  strAttr "max"
media                        =  strAttr "media"
svg_method                   =  strAttr "method"
svg_min                      =  strAttr "min"
mode                         =  strAttr "mode"
svg_name                     =  strAttr "name"
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
path_                        =  strAttr "path"
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
shape_rendering               =  strAttr "shape-rendering"
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
svg_string                   =  strAttr "string"
stroke                       =  strAttr "stroke"
stroke_dasharray             =  strAttr "stroke_dasharray"
stroke_dashoffset            =  strAttr "stroke-dashoffset"
stroke_linecap               =  strAttr "stroke-linecap"
stroke_linejoin              =  strAttr "stroke-linejoin"
stroke_miterlimit            =  strAttr "stroke-miterlimit"
stroke_opacity               =  strAttr "stroke-opacity"
stroke_width                 =  strAttr "stroke-width"
svg_style_                   =  strAttr "style"
surfaceScale                 =  fltAttr "surfaceScale"
systemLanguage               =  strAttr "systemLanguage"
tableValues                  =  strAttr "tableValues"
svg_target                   =  strAttr "target"
targetX                      =  fltAttr "targetX"
targetY                      =  fltAttr "targetY"
text_anchor                  =  strAttr "text-anchor"
text_decoration              =  strAttr "text-decoration"
text_rendering               =  strAttr "text-rendering"
textLength                   =  strAttr "textLength"
svg_title_                   =  strAttr "title"
to                           =  fltAttr "to"
transform                    =  strAttr "transform"
svg_type                     =  strAttr "type"
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
svg_version                  =  fltAttr "version"
vert_adv_y                   =  fltAttr "vert_adv_y"
vert_origin_x                =  fltAttr "vert_origin_x"
vert_origin_y                =  fltAttr "vert_origin_y"
viewBox                      =  strAttr "viewBox"
viewTarget                   =  strAttr "viewTarget"
visibility                   =  strAttr "visibility"
svg_width                    =  strAttr "width"
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
xml_space                    =  strAttr "xml:Space"
y                            =  strAttr "y"
y1                           =  strAttr "y1"
y2                           =  strAttr "y2"
yChannelSelector             =  strAttr "yChannelSelector"
z                            =  fltAttr "z"
zoomAndPan                   =  strAttr "zoomAndPan"
