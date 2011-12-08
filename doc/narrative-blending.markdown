Title: Narrative Description of "blending" Demo

This is a simple narrative description of the `blending` application included
with the Lumen library as a demo.

The `blending` demo accepts two command-line parameter, which are the names of
two image files in any format Lumen supports.  The `Crate.bmp` and `Glass.bmp`
images were added to support this demo, but any two images will work, though
maybe not be as pretty.  You invoke `blending` by typing its name followed by
the two image names, like `bin/blending data/Crate.bmp data/Glass.dmp`.  When
run, `blending` creates a window with the two images "blended" in it, starting
with the default parameters.  The images are offset; the overlapping area is
where they are blended together.

The window should have a black background with the blended images overlapping.
With the default parameters, one or both of the images might not be
immediately visible, but they're there.  Terminate the app by pressing the
Escape key, or by closing the window.

While it is running, you can cycle through the various blending equations
(add, subtract, reverse subtract, min, max) by pressing the "e" key.  You can
cycle through the source and destination factors (zero, one, color, source
alpha, and many more--consult the OpenGL documentation for the glBlendFunc
routine) by pressing the "s" key to cycle source, and the "d" key to cycle the
destination.  The app prints out a summary of the parameters it is using onto
the terminal where it was started.  This can be a useful tool for quickly
seeing what two images will look like when blended with a given set of
parameters.

The `blending` demo uses the single-handler
[`Lumen.Events.Animate.Receive_Events`][events] call for its event loop.  It
doesn't do animation, though it does use double buffering.  Ask the author
why, because I don't know!

[events]:     narrative-lumen-events.html#lumen-events
