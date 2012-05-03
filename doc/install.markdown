Title: Getting and Installing Lumen

## Requirements

To build Lumen itself, you'll need an Ada compiler that supports Ada
2005.  One good free compiler is [GNAT][] from AdaCore Technologies.
Yes, that's all!  The demos now use the "streamlined" OpenGL bindings
used by Lumen internally.  The library also includes some thin OpenGL
bindings, though we discourage their direct use; either use the
streamlined bindings (Lumen.GL, GLU, and soon to come, GLExt), or wait
for our fancy "thick" Lumen.OpenGL bindings.

To link the demos, you'll need the "development" versions of the X11 and
OpenGL libraries; on Debian and presumably Ubuntu, the package names are
`libx11-dev`, `libgl1-mesa-dev` and `libglu1-mesa-dev`.  These are apparently
only needed to create some crucial softlinks, but needed they are, so be sure
to install them if you want to build the demos.  (And trust us, you *do* want
to build the demos!)  To *run* the demos, your system must have actual GLX
support; you can verify this by running the `glxinfo` utility.  Pretty much
anything other than an error message means you're good to go.

(NOTE: As of this writing, May 2011, Lumen does use one GNAT-specific
library routine, so it is currently dependent on GNAT.  It's a very
trivial task being done, though, and we're working on replacing it
with in-line code.  No reports have come in of anyone trying to build
it with another compiler, though, so it's not a priority.)


## Fetching

A preliminary form of the Lumen library, and some demos for it, can be fetched
using this [git][] command:

        git clone https://github.com/karakalo/lumen.git

The repository is also replicated for historical reasons to NiEstu, so as an
alternative to the above you can use this:

        git clone git://ftp.niestu.com/lumen

That will create a subdirectory named `lumen` containing the library,
its demos, and its docs.


## Building Lumen

### Windows Prerequisites

In order to link the Lumen OpenGL binding on Windows, you need to have
OpenGL32.lib and Gdi32.lib in your path. You can acquire these files as part of the
[Microsoft Windows SDK](http://msdn.microsoft.com/en-us/windows/bb980924).
Another option is to build them yourself from OpenGL32.dll and Gdi32.dll
following the
[GNAT Users's Guide](http://gcc.gnu.org/onlinedocs/gcc-4.1.2/gnat_ugn_unw/Creating-an-Import-Library.html).

As soon as you have the file in your path, you can move on to the next step.

### Compiling

You need to tell Lumen which operating system you're on. This is done by
setting the scenario variable **OS**. It currently supports three values:

 * `Linux`: Compiles Lumen for a Linux / X-Server based system
 * `MacOSX`: Compiles Lumen for MacOSX
 * `Windows`: Compiles Lumen for Windows

Please substitute **$OS** with the value representing your operating system
in the following commands.

For compiling Lumen itself, do:

		cd lumen
		gnatmake -p -P lumen.gpr -XOS=$OS

That should create `liblumen.a` and a clutter of `.ali` files in the
`lib` directory.

---
NOTE: If you're using the
[MinGW GNAT compiler from GnuAda](http://gnuada.sourceforge.net/pmwiki.php/Install/MinGW),
you might get an error message

		opengl.gpr:1:01: "project" expected

(Because the gnatmake version is not new enough to support the
`library project` feature.) In this case, substitute gnatmake with gprbuild,
which does support it.

---

If you want to build the optional joystick support
as well (note that it is Linux-specific), this command should do it:

        gprbuild -P joy.gpr -XOS=Linux

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

        gnatmake -p -P demos.gpr -XOS=$OS

That should create various executables in the `bin` directory, which
you can run according to the instructions on their respective
description pages.

Lumen now also includes the first few lessons from the
[NeHe OpenGL tutorials][nehe], which can be built thusly:

        gnatmake -P nehe.gpr - XOS=$OS

This should create more executables in the `bin` directory.


## Future

That's all for now, but more is coming soon!


[demos]:   narrative-demos.html
[git]:     http://git-scm.com/
[gnat]:    http://libre.adacore.com/libre/
[holm]:    http://adaopengl.sourceforge.net/
[mesa]:    http://www.mesa3d.org/
[nehe]:    http://nehe.gamedev.net/
[oglada]:  http://www.niestu.com/software/oglada-0.3.tar.bz2
[opengl]:  http://www.opengl.org/
[x.org]:   http://www.x.org/wiki/
