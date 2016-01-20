element.decriptions<-list(
animate = c(
    title="An Element used to change an attribute over time",
    description="The animate element is put inside a shape element and defines how an attribute of an element changes over the animation. The attribute will change from the initial value to the end value in the duration specified."
),
animateMotion = c(
    title="Element used to move a shape along a path",
    description="The animateMotion element causes a referenced element to move along a motion path."
),
animateTransform = c(
    title="Element used to scale, rotate, translate and skew a shape over time",
    description="The animateTransform element animates a transformation attribute on a target element, thereby allowing animations to control translation, scaling, rotation and/or skewing."
),
mpath = c(
    title="Element used to specify path for animateMotion",
    description="the mpath sub-element for the <animatemotion> element provides the ability to reference an external <path> element as the definition of a motion path."
),
set = c(
    title="Element used to sets an attribute value at a given time",
    description="The set element provides a simple means of just setting the value of an attribute for a specified duration. It supports all attribute types, including those that cannot reasonably be interpolated, such as string and boolean values. The set element is non-additive. The additive and accumulate attributes are not allowed, and will be ignored if specified."
),
animateColor = c(
    title='Element used to specify a color transformation over time',
    description=
    "Element used to specify a color transformation over time. The use of 'animateColor' is deprecated, since all of its functionality can be achieved simply by using 'animate' to target properties that can take color values. The 'animateColor' element may be dropped from a future version of the SVG specification."
),
circle = c(
    title="Element used to create a circlular shape",
    description="The circle element is an SVG basic shape, used to create circles based on a center point and a radius."
),
ellipse = c(
    title="Element used to create an elliptic shape",
    description="The ellipse element is an SVG basic shape, used to create ellipses based on a center coordinate, and both their x and y radius."
),
line = c(
    title="Element used to create  a line (segment), shape",
    description="The line element is an SVG basic shape, used to create a line connecting two points."
),
polygon = c(
    title="Element used to create a polygon shape",
    description="The polygon element defines a closed shape consisting of a set of connected straight line segments."
),
polyline = c(
    title="Element used to create a  polyline (connected line segments), shape",
    description="The polyline element is an SVG basic shape, used to create a series of straight lines connecting several points. Typically a polyline is used to create open shapes"
),
rect = c(
    title="Element used to create a a rectangle shape",
    description="The rect element is an SVG basic shape, used to create rectangles based on the position of a corner and their width and height. It may also be used to create rectangles with rounded corners."
),
a = c(
    title='Element used to indicate web links',
    description="Container element used to create web links"
),
marker = c(
    title="Element to create a marker arrowheads, tails, ...",
    description="The marker element defines the graphics that is to be used for drawing arrowheads or polymarkers on a given <path>, <line>, <polyline> or <polygon> element."
),
mask = c(
    title="Element used to do masking",
    description="In SVG, you can specify that any other graphics object or <g> element can be used as an alpha mask for compositing the current object into the background. A mask is defined with the mask element. A mask is used/referenced using the mask property."
),
pattern = c(
    title="Element to create a specified pattern (used as fill),",
    description="A pattern is used to fill or stroke an object using a pre-defined graphic object which can be replicated (\"tiled\"), at fixed intervals in x and y to cover the areas to be painted. Patterns are defined using the pattern element and then referenced by properties fill and stroke on a given graphics element to indicate that the given element shall be filled or stroked with the referenced pattern."
),
"switch" = c(
    title="Element used as a switch description=",
    description="The switch element evaluates the requiredFeatures, requiredExtensions and systemLanguage attributes on its direct child elements in order, and then processes and renders the first child for which these attributes evaluate to true. All others will be bypassed and therefore not rendered. If the child element is a container element such as a <g>, then the entire subtree is either processed/rendered or bypassed/not rendered."
),
defs = c(
    title='Element used to contain definitions',
    description="SVG allows graphical objects to be defined for later reuse. It is recommended that, wherever possible, referenced elements be defined inside of a defs element. Defining these elements inside of a defs element promotes understandability of the SVG content and thus promotes accessibility. Graphical elements defined in a defs will not be directly rendered. You can use a <use> element to render those elements wherever you want on the viewport."
),
symbol = c(
    title='Element used to contain but not render directly a graphical object',
    description="A place to define a graphical object for later use by referencing it."
),
desc = c(
    title='Element to contain a description',
    description="Each container element or graphics element in an SVG drawing can supply a desc description string where the description is text-only. When the current SVG document fragment is rendered as SVG on visual media, desc elements are not rendered as part of the graphics. Alternate presentations are possible, both visual and aural, which display the desc element but do not display path elements or other graphics elements. The desc element generally improve accessibility of SVG documents"
),
metadata = c(
    title='A element that contains metadata',
    description="Metadata is structured data about data. Metadata which is included with SVG content should be specified within metadata elements. The contents of the metadata should be elements from other XML namespaces such as RDF, FOAF, etc."
),
title = c(
    title='Element that holds the title',
    description="Each container element or graphics element in an SVG drawing can supply a title description string where the description is text-only. When the current SVG document fragment is rendered as SVG on visual media, title element is not rendered as part of the graphics. However, some user agents may, for example, display the title element as a tooltip. Alternate presentations are possible, both visual and aural, which display the title element but do not display path elements or other graphics elements. The title element generally improve accessibility of SVG documents"
),
feBlend = c(
    title='A Filter Primitive Element used  to blend',
    description="The feBlend filter composes two objects together ruled by a certain blending mode. This is similar to what is known from image editing software when blending two layers. The mode is defined by the mode attribute."
),
feColorMatrix = c(
    title='A Filter Primitvie Element for modifing  color based on a transformation matrix',
    description="This filter changes colors based on a transformation matrix. Every pixel's color value (represented by an [R,G,B,A] vector), is matrix multiplied to create a new color."
),
feComponentTransfer = c(
    title='A Filter Primitvie Element for modifing  color using feFunc elements ',
    description="The color of each pixel is modified by changing each channel (R, G, B, and A), to the result of what the children <fefuncr>, <fefuncb>, <fefuncg>, and <fefunca> return."
),
feComposite = c(
    title='A Filter Primitive Element that composes images point-wise',
    description="This filter primitive performs the combination of two input images pixel-wise in image space using one of the Porter-Duff compositing operations: over, in, atop, out, xor. Additionally, a component-wise arithmetic operation (with the result clamped between [0..1]), can be applied."
),
feConvolveMatrix = c(
    title='A Filter Primitive Element that performs image convolution',
    description="the feConvolveMatrix element applies a matrix convolution filter effect. A convolution combines pixels in the input image with neighboring pixels to produce a resulting image. A wide variety of imaging operations can be achieved through convolutions, including blurring, edge detection, sharpening, embossing and beveling."
),
feDiffuseLighting = c(
    title='A Filter Primitive Element defining a lighting model for  diffuse  light',
    description="This filter primitive lights an image using the alpha channel as a bump map. The resulting image, which is an RGBA opaque image, depends on the light color, light position and surface geometry of the input bump map."
),
feDisplacementMap = c(
    title='A Filter Primitive Element that creating displacement',
    description="This filter primitive uses the pixels values from the image from in2 to spatially displace the image from in."
),
feFlood = c(
    title='A Filter Primitive Element that floods with a color',
    description="The filter fills the filter subregion with the color and opacity defined by flood-color and flood-opacity."
),
feGaussianBlur = c(
    title='A Filter Primitive Element to provide a blurring effect',
    description="The filter blurs the input image by the amount specified in stdDeviation, which defines the bell-curve."
),
feImage = c(
    title='A Filter Primitive Element to fetch image data',
    description="The feImage filter fetches image data from an external source and provides the pixel data as output (meaning, if the external source is an SVG image, it is rasterize),."
),
feMerge = c(
    title='A Filter Primitive Element to merge images',
    description="The feMerge filter allows filter effects to be applied concurrently instead of sequentially. This is achieved by other filters storing their output via the result attribute and then accessing it in a <femergenode> child."
),
feMorphology = c(
    title='A Filter Primitive Element to erode or dilate',
    description="This filter is used to erode or dilate the input image. It's usefulness lies especially in fattening or thinning effects."
),
feOffset = c(
    title='A Filter Primitive Element to offset an image',
    description="The input image as a whole is offset by the values specified in the dx and dy attributes. It's used in creating drop-shadows."
),
feSpecularLighting = c(
    title='A Filter Primitive Element defining a lighting model for  specular  light',
    description="This filter primitive lights a source graphic using the alpha channel as a bump map. The resulting image is an RGBA image based on the light color. The lighting calculation follows the standard specular component of the Phong lighting model. The resulting image depends on the light color, light position and surface geometry of the input bump map. The result of the lighting calculation is added. The filter primitive assumes that the viewer is at infinity in the z direction."
),
feTile = c(
    title='Element to tile a region with a given image',
    description="An input image is tiled and the result used to fill a target. The effect is similar to the one of a <pattern>."
),
feTurbulence = c(
    title='A Filter Primitive Element producing turbulence effect on an image',
    description="This filter primitive creates an image using the Perlin turbulence function. It allows the synthesis of artificial textures like clouds or marble."
),
font = c(
    title='Element used to define the font',
    description="The font element defines a font to be used for text layout."
),
"font-face" = c(
    title='Element used to define the font face',
    description="The font-face element corresponds to the CSS @font-face declaration. It defines a font's outer properties."
),
"font-face-format" = c(
    title='An Element to define the font fac',
    description="The font-face-format element describes the type of font referenced by its parent <font-face-uri>."
),
"font-face-src" = c(
    title='An  Element to define the font face',
    description="The font-face-src element corresponds to the src property in CSS @font-face descriptions. It serves as container for <font-face-name>, pointing to locally installed copies of this font, and <font-face-uri>, utilizing remotely defined fonts."
),
"font-face-uri" = c(
    title='An  Element to define the font face',
    description="The font-face-uri element points to a remote definition of the current font."
),
hkern = c(
    title='An  Element used to specify horizontal distances between glyphs',
    description="The horizontal distance between two glyphs can be fine-tweaked with an hkern Element. This process is known as Kerning."
),
vkern = c(
    title='An  Element used to specify vertical distances between glyphs',
    description="The vertical distance between two glyphs in top-to-bottom fonts can be fine-tweaked with an vkern Element. This process is known as Kerning."
),
linearGradient = c(
    title='An Element to define a linear gradient',
    description="LinearGradient is used to define a linear gradient fill or stroke of  a graphical element."
),
radialGradient = c(
    title='An Element to define a radial gradient',
    description="RadialGradient is used to define a radial gradient fill or stroke of  a graphical element."
),
"stop" = c(
    title='An element used to define the colors and layouts of a gradient',
    description="The ramp of colors to use on a gradient is defined by the stop elements that are child elements to either the <lineargradient> element or the <radialgradient> element."
),
image = c(
    title='An Element used to include a raster image',
    description="The SVG Image Element (<image>), allows a raster image into be included in an SVG document."
),
use = c(
    title='An element used for taking another element as a template',
    description="The use element takes nodes from within the SVG document, and duplicates them somewhere else. The effect is the same as if the nodes were deeply cloned into a non-exposed DOM, and then pasted where the use element is, much like anonymous content and XBL. Since the cloned nodes are not exposed, care must be taken when using CSS to style a use element and its hidden descendants. CSS attributes are not guaranteed to be inherited by the hidden, cloned DOM unless you explicitly request it using CSS inheritance."
),
path = c(
    title='An element to create a path',
    description="A path is defined by including a 'path' element which contains a d='(path data),' attribute, where the 'd' attribute contains the moveto, line, curve (both cubic and quadratic Beziers), arc and closepath instructions."
),
text = c(
    title='An element used to create text',
    description="The text element defines a graphics element consisting of text. Note that it is possible to apply a gradient, pattern, clipping path, mask or filter to text"
),
feDistantLight = c(
    title='A Primitive Filter Element that defines a distant light source',
    description="This filter primitive define a distant light source that can be used within a lighting filter primitive : <fediffuselighting> or <fespecularlighting>."
),
fePointLight = c(
    title='A Primitive Filter Element that defines a pointlight light source',
    description=" description="
),
feSpotLight = c(
   title='A Primitive Filter Element that defines a spotlight light source',
    description=" description="
),
altGlyphDef = c(
    title='Element used to define a set of possible glyph substitutions',
    description="The altGlyphDef element defines a substitution representation for glyphs. This will consist of  either one or more glyphRef elements or one or more altGlyphitem elements."
),
altGlyphItem = c(
    title='Element used to provide a set of glyph substitution candidates',
    description="The altGlyphItem element provides a set of candidates for glyph substitution by the <altglyph> element."
),
glyph = c(
    title='Element used to define graphics for a given glyph',
    description="A glyph defines a single glyph in an SVG font."
),
glyphRef = c(
    'Element used to define a possible glyph to use',
    description="The glyphRef element provides a single possible glyph to the referencing <altglyph> substitution."
),
altGlyph = c(
    title='Element used to provide control over the glyphs used to render particular character data',
    description="The altGlyph element allows sophisticated selection of the glyphs used to render its child character data."
),
textPath = c(
    title='Element used to place text along a given path',
    description="In addition to text drawn in a straight line, SVG also includes the ability to place text along the shape of a <path> element. To specify that a block of text is to be rendered along the shape of a <path>, include the given text within a textPath element which includes an xlink:href attribute with a reference to a <path> element."
),
tref = c(
    title='Element used to provide a reference to text content data',
    description="The textual content for a <text> can be either character data directly embedded within the <text> element or the character data content of a referenced element, where the referencing is specified with a tref element."
),
tspan = c(
    title='Element used to control sections of text data within a text element',
    description="Within a <text> element, text and font properties and the current text position can be adjusted with absolute or relative coordinate values by including a tspan element."
),
clipPath = c(
    title='Element used to define a boundry (path), for clipping',
    description="The clipping path restricts the region to which paint can be applied. Conceptually, any parts of the drawing that lie outside of the region bounded by the currently active clipping path are not drawn."
),
"color-profile" = c(
    title='Element used to specify color profiles',
    description="The element allows describing the color profile used for the image."
),
cursor = c(
    title='Element used to define platform-independent custom cursor',
    description="The cursor element can be used to define a platform-independent custom cursor. A recommended approach for defining a platform-independent custom cursor is to create a PNG image and define a cursor element that references the PNG image and identifies the exact position within the image which is the pointer position (i.e., the hot spot),."
),
feFuncA = c(
    title='Element used to specify alpha component transfer function',
    description="This filter primitive defines the transfer function for the alpha component of the input graphic of its parent <fecomponenttransfer> element."
),
feFuncB = c(
    title='Element used to specify blue component transfer function',
    description="This filter primitive defines the transfer function for the blue component of the input graphic of its parent <fecomponenttransfer> element."
),
feFuncG = c(
    title='Element used to specify green component transfer function',
    description="This filter primitive defines the transfer function for the green component of the input graphic of its parent <fecomponenttransfer> element."
),
feFuncR = c(
    title='Element used to specify red component transfer function',
    description="This filter primitive defines the transfer function for the red component of the input graphic of its parent <fecomponenttransfer> element."
),
feMergeNode = c(
    title='Element used to wrap a filter primitive prior to being used by femerge',
    description="The feMergeNode takes the result of another filter to be processed by its parent <femerge>."
),
filter = c(
    title='Container element used to modify an image',
    description="The filter element serves as container for atomic filter operations. It is never rendered directly. A filter is referenced by using the filter attribute on the target SVG element."
),
"font-face-name" = c(
    title='Element used to specify a local font-face by name',
    description="The font-face-name element points to a locally installed copy of this font, identified by its name. The font-face element is used within a font-face-src element."
),
foreignObject = c(
    title='Element used to include a non-svg element',
    description="The foreignObject element allows for inclusion of a foreign XML namespace which has its graphical content drawn by a different user agent. The included foreign graphical content is subject to SVG transformations and compositing."
),
"missing-glyph" = c(
    title='Element used to define the graphics to use when an attempt to draw a glyph that has not been defined.',
    description="The missing-glyph's content is rendered, if for a given character the font doesn't define an appropriate <glyph>."
),
script = c(
    title='Element used to embed javascript',
    description="Any functions defined within any script element have a global scope across the entire current document."
),
style = c(
    title='Element used to embed a style specification',
    description="The style element allows style sheets to be embedded directly within SVG content. SVG's style element has the same attributes as the corresponding element in HTML (see HTML's <style> element),."
),
view = c(
    title='Element used to change the appearance of the content of an svg element it contains',
    description="A view is a defined way to view the image, like a zoom level or a detail view."
)
)
