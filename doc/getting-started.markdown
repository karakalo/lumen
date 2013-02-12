Title:  Getting Started With Lumen

[TOC]

Welcome to Lumen!  It has aspirations to become a complete GUI toolkit,
replacing venerable mainstays such as [GTK+][gtk], [Qt][], [TCL/Tk][tk], or
more properly, [GTKAda][], [QtAda][], or [TASH][], as it is targeted
specifically to Ada applications.  At the moment, it's just an infrastructure
library for OpenGL, more akin to [GLFW][].  But improvements are planned!

## Get Going


### Fetching and Building

First off, you need to fetch the source for the library and its demo apps, and
build it all.  This process is described in detail on the
[Getting and Installing Lumen][install] page.  If you have any trouble
following those instructions, or any trouble running the demos or your own
apps, the quickest way to get help is to come to the \#Ada IRC channel on
[Freenode][] and ask.  Or submit an [issue on github][issues] if IRC isn't
your bag.


### Running the Demos

Some developers have had trouble running some of the demos, such as `text1`,
`text2`, and `multi`, and some of the later NeHe demos.  This was because they
changed their current working directory into `bin` before running those demos,
and thus the demo could not find a crucial data file it needed:

        $ cd bin
        $ ./text1

        raised TEXT1.PROGRAM_ERROR : cannot find default font file "fsb.txf"

Or this:

        $ cd bin
        $ ./lesson06

        raised LUMEN.BINARY.IO.NONEXISTENT_FILE : lumen-binary-io.adb:51

That's because the demo apps are hard-coded to look for files that are kept in
the `data` subdir.  There are several solutions to this problem, but the
simplest one, and the one we recommend, is to stay in the `lumen-demos`
directory and run the demos like, in the above examples, `bin/text1` or
`bin/lesson06`.

This has nothing to do with Lumen itself; the demos are just coded in a
simple-minded way.  We wanted to demonstrate Lumen, not how to find files in
Ada.  Some have suggested that we enhance the demos to support at least this
(apparently common, if you're an MS-Win user) case; we're taking that under
advisement.


### First Demos

You can exit any of the demos described below by hitting the Escape key.

#### The `info` Demo

Now that your Lumen is built, probably the first demo you should run, by way
of checking things out, is `info`.  It will open a window, that will
immediately close (making it "flash"--this is expected, even though it looks
like an error), and will print out certain information it gets from the GPU:

        $ bin/info
        OpenGL version:           4.2.11762 Compatibility Profile Context
        Renderer:                 AMD Radeon HD 6670
        Vendor:                   ATI Technologies Inc.
        Shader language version:  4.20

Running `info` will accomplish two things:

1. Because it does create an actual Lumen window, even though it doesn't use
it for anything, it will serve as verification that Lumen does actually run on
your platform.  Not all problems can be caught at build time; running the code
is a useful check.

1. It will tell you what OpenGL version, and GLSL (shader language) version
your platform supports.  This is very useful, as some features, like shader
syntax and vertex buffers, behave differently or won't run at all on certain
versions.

Also, if you give `info` a non-null command-line argument, it will in addition
print out a list of OpenGL extensions your hardware supports:

        $ bin/info x
        OpenGL version:           4.2.11762 Compatibility Profile Context
        Renderer:                 AMD Radeon HD 6670
        Vendor:                   ATI Technologies Inc.
        Shader language version:  4.20
        Extensions:
           GL_AMDX_debug_output
           GL_AMDX_vertex_shader_tessellator
           GL_AMD_conservative_depth
           ... and on and on ...
           GL_SUN_multi_draw_arrays
           GL_WIN_swap_hint
           WGL_EXT_swap_control

This list is usually quite long:  On the development system here, it's 215
lines.  So it's usually used like this:

        $ bin/info x | grep some-extension-i'm-looking-for

Or at least redirect it to a file or pipe it to `more`.

**NOTE**: If you get a `Not_Available` exception when running `info`, or any
other Lumen application, that could mean that your OpenGL implementation can't
support the default "visual", a set of attributes that describe certain
operating limits.  See the [Troubleshooting](#troubleshooting) section below
for details of the defaults, and the `attribs` demo section below for help
finding out what your system supports.

#### The `attribs` Demo

[![attribs demo](images/attribs-thumb.png)](images/attribs.png)

The `attribs` demo is another utility demo, designed to provide information
about your drawing environment.  It can also help in diagnosing X11 visual
selection problems, as described below in the
[Troubleshooting](#troubleshooting) section.  If any Lumen app is failing to
run, and raising a `Not_Available` exception, you can try this:

        bin/attribs c0 a0 d0 s0 n

If that works, you can either just use the visual Lumen selects using those
values, which will mean you can add them to the end of a few demo commands and
get them to run, or follow the steps in the Troubleshooting section to find a
more suitable visual, which you can select using the `LUMEN_VISUAL_ID`
environment variable.

These days it is rare for Lumen to be unable to find a suitable visual to use,
but it can happen.

#### The `texture` Demo

[![texture demo](images/texture-thumb.png)](images/texture.png)

The `texture` demo is the first one here with any kind of eye appeal.  It
simply renders an image onto a quadrangle, then rotates it.  You pass the
pathname of the image file as a command-line argument:

        bin/texture data/ppm-test-8.ppm

That's a pretty picture from Alaska.  Any of the PPM or BMP files in `data`,
or in theory anywhere on your system, can be used.  If you find one that
doesn't work, please [create an issue][issues] so we can fix it.

The `texture` demo is also one of the few that accepts the a, c, d, n, and s
arguments to adjust the visual settings (see the
[troubleshooting](#troubleshooting) section), allowing you to run at least one
"normal" demo on lower-end hardware without modifying the source.

#### The `fire` Demo

[![fire demo](images/fire-thumb.png)](images/fire.png)

The `fire` demo is worth a look, first because it is pretty as hell, and
second because it reports its framerate in the widow's title bar.  Knowing the
range of framerates your system is capable of can be very important to some
people.

It accepts no arguments, and has no keyboard or mouse controls.  Just run it
and sit back:

        bin/fire


### More Demos

#### The NeHe Demos

The first six [NeHe OpenGL tutorials][nehe] are a delightful introduction to
OpenGL programming, both in code and visually.

1. [![lesson01 demo](images/lesson01-thumb.png)](images/lesson01.png)
`lesson01` draws a black screen, which isn't very impressive, but it's
something you will likely see fairly often if you do any amount of OpenGL
development.  It's what you get from an OpenGL program that doesn't draw
anything, or does draw something but does it wrong.

1. [![lesson02 demo](images/lesson02-thumb.png)](images/lesson02.png)
`lesson02` draws a triangle and a square, both white on black.  No, they
don't move or anything, but they're there.

1. [![lesson03 demo](images/lesson03-thumb.png)](images/lesson03.png)
`lesson03` ups the ante on the triangle and square by adding colors.  The
triangle is a shaded rainbow, and the square is slate blue.

1. [![lesson04 demo](images/lesson04-thumb.png)](images/lesson04.png)
`lesson04` ups the ante on the triangle and square again by making the
shapes move.

1. [![lesson05 demo](images/lesson05-thumb.png)](images/lesson05.png)
`lesson05` adds the third dimension, and turns the triangle and square into
a pyramid and a cube.

1. [![lesson06 demo](images/lesson06-thumb.png)](images/lesson06.png)
`lesson06` ditches the pyramid, but "colors" the cube using a texture
image.  This is the pinnacle of OpenGL--now you're ready to write that game
you always wanted to do!

Lessons 07 and 08 are nice too--be sure to hit the arrow keys to spin the
cube, the page keys to zoom it, and the "l" key to turn lighting on and off.

#### The `text2` Demo

[![text2 demo](images/text2-thumb.png)](images/text2.png)

The `text2` demo is a small example of fixed framerate animation, and also
displaying live data.  It's set to display at 60 frames per second, and the
second number you see should be close to 60.  You can stop the message panel's
rotation with the spacebar.

#### The Joystick Demo

There isn't a joystick demo yet, but soon!  Very soon.


### Writing Your Own Lumen Applications

There will be more text here, and more useful code in the distribution, very
soon.  For now, look at the demo sources, read the narratives
[for the library][nlumen] and [the demos][ndemos], and give it a try!


### Troubleshooting

#### The `Not_Available` Exception

Under X11, OpenGL contexts (drawing areas) use an object called a "visual",
which has certain configurable values.  The Lumen `Create` procedure lets you set
those values via the `Attributes` parameter.  Under Win32, those values are
summarily ignored, so don't bother.

Most of the demos use a set of default values for these attributes; the values
of those defaults are:

 * red, green, and blue size     =>  8
 * alpha size                    =>  8
 * depth size                    => 24
 * stencil size                  =>  8

If your system doesn't support all of these default values, Lumen will raise a
`Not_Available` exception when trying to create a window.  That's not the only
reason for this exception, but it is the most common one.

Now, some of the demos allow you to override these defaults using command-line
parameters.  The demos that allow that are:

 * attribs
 * multi
 * text2
 * texture

We recommend the `attribs` demo for this purpose, as that is the reason it was
written.  To try a lower stencil size:

        bin/attribs s0

To try a very low value for all:

        bin/attribs c0 a0 d0 s0 n

Supposedly using zero for the value chooses "the lowest value supported".

There is a last-ditch low-rent emergency workaround for the visual-choosing
problem.  First, run this command:

        glxinfo -v | more

Scroll down until you get into the "GLX Visuals" section, and browse until you
see a nice one.  It will need to match the color depth used by your app (all
the demos try to use TrueColor), and if you want to do animation, will need to
support double buffering ("doubleBuffer=1").  Note the visual's ID, and put
that value into an environment variable like so:

        LUMEN_VISUAL_ID=4e   # 4e is just an example, use a value from your own glxinfo
        export LUMEN_VISUAL_ID

In addition, the chosen visual's capabilities must be met by Lumen, so:

        $ LUMEN_VISUAL_ID=4e bin/attribs s4

Is an example of choosing, and then using, a visual with a smaller stencil size.

#### Other Problems

We get email when you file an issue on the [github issue tracker][issues].
The quickest way to get help is to come to the \#Ada IRC channel on
[Freenode][] and ask.


[freenode]:  http://freenode.net/
[glfw]:      http://www.glfw.org/
[gtk]:       http://www.gtk.org/
[gtkada]:    http://libre.adacore.com/tools/gtkada/
[install]:   install.html
[issues]:    https://github.com/karakalo/lumen/issues
[ndemos]:    narrative-demos.html
[nehe]:      http://nehe.gamedev.net/
[nlumen]:    narrative-lumen.html
[qt]:        http://qt-project.org/
[qtada]:     http://www.qtada.com/
[tash]:      http://tcladashell.sourceforge.net/
[tk]:        http://www.tcl.tk/
