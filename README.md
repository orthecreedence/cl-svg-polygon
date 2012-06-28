DOCUMENTATION NOT FINISHED YET!
===============================

cl-svg-polygon
==============

(Or svgp for short). This is a library that takes an SVG file and generates, for each SVG object, a
set of points that outline the object. It approximates curves based on a given resolution: the 
higher the resolution, the more accurate the curve is (this works for paths, circles, ellipses).
This package relies on [xmls](http://common-lisp.net/project/xmls/) to parse the SVG file's XML.

Usage
-----
Parse the contents of an SVG string:
    
    (svgp:parse-svg-string "...")

Parse the contents of an SVG file:

    (svgp:parse-svg-file "drawing.svg")   ; wraps around parse-svg-string

Both these functions return a list of SVG objects (plists):

