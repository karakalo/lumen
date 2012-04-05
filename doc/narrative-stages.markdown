Title: Narrative Description of "simple_2d" and "simple_3d" Demos

This is a simple narrative description of the "stage" demo applications
included with the Lumen library as a demo.  The stage demos are called
`simple_2d` and `simple_3d`.

The stage demos started life as "template" apps for OpenGL, empty skeletons
into which you can put any OpenGL drawing you want, for those times you just
want to see the rendering and not worry about setting it up.  The original
versions were written in Python and Ada, and used the Gtk+ toolkit.  These
Lumen adaptations of the apps are still works in progress, but are useful
enough already that they're being included in case someone else might find
them useful, rather than letting them collect dust on a shelf.

The name "stage" is meant in the sense of a theatre stage, a place where you
can show and see your productions.  The intent is, you make a copy of the
source and search for the string "here:" for the two places to insert your own
OpenGL code.  One place is for initialization, and the other is for the actual
drawing code.  Build and go.  The separate distribution package for the stage
demos is [available here][stages], though now that they're being included as
Lumen demos that package is unlikely to see future updates.

You invoke `simple_2d` or `simple_3d` by typing its name, like `./simple_2d`
if you're sitting in the `bin` directory, or `bin/simple_3d` if you're in the
`lumen` directory.

When run, the stage demos create a black window with a red X and green Y axis,
adding a blue Z axis for the `simple_3d` version.  For the 2D version, hold
mouse button 1 and drag to pan the view, button 2 to rotate, or button 3 to
scale.  You can also scale the view using the mouse wheel.  Hit the "Home" key
to reset the pan, scale, and rotation to their initial defaults.  The 3D demo
does rotation instead of panning when you hold button 1, but otherwise the
same controls apply.  Terminate the app by pressing "q" or the Esc key, or by
closing the window.

[stages]:   http://www.niestu.com/software/stages-1.2.tar.gz
