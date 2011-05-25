Title: Narrative Description of "text2" Demo

This is a simple narrative description of the `text2` application included
with the Lumen library as a demo.

The `text2` demo can accept several optional command-line parameters.  The
first param is always the pathname of a texture-mapped font file; the second
and subsequent params are values used to set the attributes used to select an
OpenGL visual.  If you omit the pathname, it uses `fsb.txf` by default; if any
or all of the attribute values are omitted, it uses the same values as the
library's defaults.  Full documentation of the command line parameters can be
found on [the demo "manual" page][manual].

You invoke `text2` by typing its name, optionally followed by a font-file
pathname, and further optionally followed by attribute override values, like
`./text2 ../data/fsb.txf` if you're sitting in the `bin` directory, or
`bin/text2 data/fsb.txf a0 d16` if you're in the `lumen` directory and want to
select a visual different from the default, in this case one with 0 bits
required for an alpha channel and 16 bits in the depth buffer.  (Or, of
course, simply as `./text2` or `bin/text2`, to use the default font and
attributes.)  The `fsb.txf` and `chopin.txf` files are two sample
texture-mapped fonts provided for the demo programs; you can create others
using the `tools/make-txf` tool.  The "Chopin" sample font will *work* with
`text2`, but it will look like ass.  The FreeSans Bold font is monospaced, but
Chopin is proportional, causing the animated text displayed by `text2` to jump
around crazily with every frame.

When run, `text2` creates a light grey window with a rotating black square in
the middle, which should be about half the width of the window itself.  On the
square, actual live program data is displayed in green letters: the frame
number, and the current framerate, which should be close to 60.  You can speed
up or slow down the rotation of the square using the Up and Down arrow keys,
or pause/restart it with the spacebar.  Note that the updating of the
displayed data is independent of the square's rotation.  Terminate the app by
pressing "q" or the Esc key, or by closing the window.

The `text2` demo started life as a clone of the [`texture`][texture] demo, but
has code added to accept command-line parameters, to display program data as
text, and to accept various simple keyboard commands.  It was intended as a
"stretch the legs" demo for Lumen, to do the sort of stuff real apps do,
although as with [`text1`][text1] and most of the other demos before it, it
uses lower-level calls than the eventual Lumen app would use.  Demos for the
more advanced services will come when those services are written.

[texture]:  narrative-sgi_simple.html
[text1]:    narrative-text1.html
[manual]:   demo-manual.html
