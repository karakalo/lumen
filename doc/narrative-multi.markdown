Title: Narrative Description of "multi" Demo

This is a simple narrative description of the `multi` application included
with the Lumen library as a demo.

The `multi` demo can accept several optional command-line parameters,
identical to those accepted by the `text2` demo.  Full documentation of the
command line parameters can be found on [the demo "manual" page][manual].

You invoke `multi` by typing its name, optionally followed by a font-file
pathname, and further optionally followed by attribute override values, like
`./multi ../data/fsb.txf` if you're sitting in the `bin` directory, or
`bin/multi data/fsb.txf a0 d16` if you're in the `lumen` directory and want to
select a visual different from the default, in this case one with 0 bits
required for an alpha channel and 16 bits in the depth buffer.  (Or, of
course, simply as `./multi` or `bin/multi`, to use the default font and
attributes.)  The `fsb.txf` and `chopin.txf` files are two sample
texture-mapped fonts provided for the demo programs; you can create others
using the `tools/make-txf` tool.  The "Chopin" sample font will *work* with
`multi`, but as with [`text2`][text2] it will look awful.

When run, `multi` creates two windows, a scene window and a data window.  The
scene window has a light grey background with a rotating blue and red sphere.
The data window is black, with the frame number and the current framerate
(which should be close to 30) displayed in green.  You can speed up or slow
down the rotation of the sphere using the Up and Down arrow keys,
pause/restart it with the spacebar, or set it back to its default with the
equals sign.  Note that the updating of the displayed data is independent of
the sphere's rotation.  Terminate the app by pressing "q" or the Esc key, or
by closing either window.

The `multi` demo started life as a clone of the [`text2`][text2] demo, but has
code added to open two separate windows.  It shows how to create and manage
the two windows, using [the `Lumen.Window.Make_Current` call][window] to
select which window gets the OpenGL activity at any given point.  One aspect
of its behavior may not be readily evident: Event handling is attached to the
scene window, because the events mechanism needs some window to use, but
keyboard events are processed the same regardless of which window has focus.
We're not entirely sure why it works that way, and are not sure it's the way
we want it to work, so keep an eye out for changes in this area.

[text2]:    narrative-text1.html
[manual]:   demo-manual.html
[window]:   narrative-lumen-misc.html#lumen-window
