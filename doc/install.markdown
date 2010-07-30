Title: Getting and Installing Lumen

## Requirements

To build Lumen itself, you'll need an Ada compiler that supports Ada 2005.
One good free compiler is [GNAT][] from AdaCore Technologies.  You'll also
need [OpenGL][] development libraries on your system; most free software
systems have [Mesa][] or something like it available.  Right now, Lumen uses
the X window system, so you'll need development libraries for that too; for
most free systems, that means [X.org][].  (**NOTE**: It says "development
libraries" for a reason: Most free software distributions package the runtime
libraries separately from the ones needed for software development, so make
sure you have both.  For example, on Debian Linux, the X runtime library
package is called `libx11-6`, and the development library package is
`libx11-dev`.)

To build the demos, you'll also need some Ada OpenGL bindings.  The
[ones I use][oglada] are very old, but they still work.  Some slightly newer
ones (but still pretty old) are [the ones by David Holm][holm] at
SourceForge.  The intent is to distribute some new up-to-date fancy OpenGL
bindings as part of Lumen; coming soon!

## Fetching

A preliminary form of the Lumen library, and some demos for it, can be fetched
using this [git][] command:

        git clone git://ftp.niestu.com/lumen

That will create a subdirectory named `lumen` containing the library, its
demos, and its docs.


## Building Lumen

Once you have downloaded the source, these commands will build Lumen itself:

        cd lumen
        gnatmake -P lumen.gpr

That should create `liblumen.a` and a clutter of `.ali` files in the `lib`
directory.  If you want to build the joystick support as well, this command
should do it:

        gprbuild -P joy.gpr

The `gprbuild` command has some configuration it likes, and if that's not set
up on your system yet, it may give you some static.  The program `gprconfig`
lets you set that up, choosing which compilers and languages to use for
builds.  Pick at least a C compiler and an Ada compiler from its menu, and
save the config, after which the above command *should* work.

The command should add `lumen-joystick.ali` and `liblumenjoy.a` to the `lib`
directory, which will allow you to build the joystick demos.  And if you have
a joystick or a game pad or something similar, you should be able to actually
*run* them!

## Building the Demos

Once you have built the library, you should be able to build
[the demo programs][demos] with this command:

        gnatmake -P demos.gpr

That should create various executables in the `demo` directory, which you can
run according to the instructions on their respective description pages.

## Future

That's all for now, but more is coming soon!


[git]:     http://git-scm.com/
[gnat]:    http://libre.adacore.com/libre/
[mesa]:    http://www.mesa3d.org/
[opengl]:  http://www.opengl.org/
[x.org]:   http://www.x.org/wiki/
[oglada]:  http://www.niestu.com/software/oglada-0.3.tar.bz2
[holm]:    http://adaopengl.sourceforge.net/
[demos]:   narrative-demos.html
