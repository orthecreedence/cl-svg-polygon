cl-svg-polygon
==============
(Or svgp for short). This is a pure-lisp library that takes an SVG file and generates, for each SVG object, a
set of points that outline the object. It approximates curves based on a given resolution: the 
higher the resolution, the more accurate the curve is (this works for paths, circles, ellipses).
This package relies on [xmls](http://common-lisp.net/project/xmls/) to parse the SVG file's XML.

cl-svg-polygon also calculates transformations within the groups and objects in the file to the
final points/holes returned for each polygon.

Usage
-----
Parse the contents of an SVG string:
    
    (svgp:parse-svg-string "...")

Parse the contents of an SVG file:

    (svgp:parse-svg-file "drawing.svg")   ; wraps around parse-svg-string

Both these functions return a flat list of SVG objects (plists):

    '((:type "path"
       :group ("layer1" "ghostie" "g221")
       :d "m -397.936,580.106 c -0.294,-1.122 -1.382,-2.535 -2.729,-2.046 -1.52,0.551 -2.286,1.984 ..."
       :style "fill:#ffffff"
       :point-data #((400.94077 477.2889) (400.82928 476.94577) (400.67334 476.5983) (400.4762 476.25974) ...)
       :holes (#((400.94077 477.2889) (400.82928 476.94577) (400.67334 476.5983) (400.4762 476.25974) ...))
	   :meta (:disconnected nil)) ...)

### Call parameters
Here are the keyword parameters you can give the the (parse-svg-\* ...) functions:

    :curve-resolution    ; How many points each curve will use. Higher == more accurate
    :scale               ; Apply a scale vector '(x y) to ALL points/holes in all objects returned. Can be used with a negative value to invert the y axis.

### Grouping
Even though the objects in the file may be in group hierarchies, they are returned as a flat list.
The `:group` key contains *each* group the object is a member of, ordered from the top down. This
can be used to create hierarchies among the objects later on if needed.

### Polygon data
The `:point-data` key contains the outline of the polygon as a vector of points `#((x1 y1) (x2 y2) ...)`
and `:holes` contains holes that are cut out of the polygon.

### Object meta information
Objects can return meta information in the final result under the `:meta` key. Currently, this is
only used for paths, which return whether or not the path is "disconnected," meaning it's a set of
line segments such that the end does *not* join up with the beginning (if `:disconnected t`).
This can be useful in determining whether or not to bother with triangulation.

More object types may use meta later on.

Philosophy
----------
The idea behind this library is that SVG files can be used to describe objects that will be sent
to OpenGL (or any other display system). With something like Illustrator or Inkscape, you could
have an instant level editor for a game.

In addition to polygons/holes, it tries to return as much relevant data about each object as
possible (groups, styles, meta info) so you can make decisions about the object later on in your
application.

Triangulation
-------------
In the spirit of doing one thing well, this library focuses no attention on turning the polygons
it churns out into triangles. Please see [glu-tessellate](http://github.com/orthecreedence/glu-tessellate)
for a triangulation library that wraps around GLU's tessellation system.
[glu-tessellate](http://github.com/orthecreedence/glu-tessellate) triangulates (with holes) and
[supports a number of winding methods](http://www.glprogramming.com/red/chapter11.html).

Limitations
-----------
There are a few limitations with this library:

 - Line/polyline types are ignored when parsing. This fix is coming soon.
 - Clipping/masking are not supported. Fix also coming soon, especially since masks appear in many
 of the files I'm parsing. For now, they are just ignored.

Notes
-----
I do my best to keep cl-svg-polygon up to date with the [SVG spec](http://www.w3.org/TR/SVG/).
I do plan on fixing the limitations in a timely manner. If you see any bugs or
problems with this library, please feel free to contact me or fix it and issue
a pull request.

Thanks!
