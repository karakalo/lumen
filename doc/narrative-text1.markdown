Title: Narrative Description of "text1" Demo

This is a simple narrative description of the `text1` application included
with the Lumen library as a demo.

The `text1` demo can accept one optional command-line parameter, which is the
pathname of a texture-mapped font file.  If you omit the pathname, it uses
`fsb.txf` by default, which is a snapshot of the FreeSans Bold font.  The
[texture-mapped font][font] mechanism is one way Lumen has of displaying text.

You invoke `text1` by typing its name, optionally followed by a font-file
pathname, like `./text1 ../data/fsb.txf` if you're sitting in the `bin`
directory, or `bin/text1 data/fsb.txf` if you're in the `lumen` directory.
(Or, of course, simply as `./text1` or `bin/text1`, to use the default font.)
The `fsb.txf` and `chopin.txf` files are two sample texture-mapped fonts
provided for the demo programs; you can create others using the
`tools/make-txf` tool.  Either of the sample fonts should work fine with
`text1`.

When run, `text1` creates a black window with a white square in the middle,
which should be about half the width of the window itself.  On the square, the
words "OpenGL does text!" should appear in dark blue letters.  Terminate it by
pressing "q" or the Esc key, or by closing the window.

Structurally the `text1` demo is a clone of the [`sgi_simple`][sgi_simple]
demo, with a bunch of code added to support calling the very low-level
texture-mapped font package.  Normally Lumen apps wouldn't use that package
directly, but it needed testing and demoing, so voila.

[sgi_simple]: narrative-sgi_simple.html
[font]:       narrative-lumen-font.html
