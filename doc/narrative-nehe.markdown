Title: Narrative Description of NeHe Demos

This is a simple narrative description of the `lessonNN` applications included
with the Lumen library as demos.  They are OpenGL tutorials taken from
[the NeHe Productions][nehe] website, graciously converted to Ada with Lumen
by the indefatigable Julian Leyh.  Since all these demos are better described
elsewhere, this single page will cover the (current) lot.  Once more advanced
lessons are contributed, we may create separate pages for those.

None of the NeHe demos accept any command-line parameters; the ones that use
separate image files (currently `lesson06`, `lesson07`, and `lesson08`) have
the pathnames hard-coded in the source.  For this reason, those must be run
from the `lumen` directory, like `bin/lesson08` or whatever.  All NeHe demos
can be terminated with the Esc key.  Full documentation of the demos can be
found on [the demo "manual" page][manual].

Here's a quick rundown of what each tutorial does:

 * `lesson01` -- Just about the simplest possible OpenGL app.  Creates a
   window and clears it to the default color, which these days is black.  This
   version is more complex than it needs to be, uselessly declaring itself as
   an animation when its display is quite static.  But it sets the stage for
   the more advanced tutorials to come.

 * `lesson02` -- Next step up the OpenGL food chain: draw some shapes, in this
   case one triangle and one "quad" (rectangle).  Black background, white shapes.

 * `lesson03` -- Add color to lesson 2, with a smooth-shaded multicolored
   triangle, and a medium-blue quad.

 * `lesson04` -- Spin the shapes from lesson 3, the triangle horizontally, the
   quad vertially.

 * `lesson05` -- Introduce 3D solids.  The triangle from lesson 4 becomes a
   pyramid, and the quad becomes a cube.  Both still spinning, though the cube
   spins on an oblique axis for a more interesting look.

 * `lesson06` -- Lose the pyramid and make the cube the center of attention.
   Now it has a texture mapped onto it, in this case the NeHe GameDev logo,
   and it tumbles on all three axes.

 * `lesson07` -- Another textured cube, this one with a nice wooden-crate
   motif.  Mash the up/down/left/right arrow keys to set it spinning,
   PgUp/PgDn to move it closer or farther away, the "l" key (lowercase L) to
   change the lighting, and the "t" key to cycle through various texture
   options.  The differences between texture settings are subtle, but they're
   visible if you look closely.

 * `lesson08` -- A more sophisticated version of lesson 7, with a
   stained-glass texture on the cube.  It accepts all the same keyboard
   commands that `lesson07` does, and adds a 'b' command to toggle blending.
   Nice translucence effect, and quite different with and without lighting
   enabled.  A very cool demo indeed.

[manual]:   demo-manual.html
[nehe]:     http://nehe.gamedev.net/
