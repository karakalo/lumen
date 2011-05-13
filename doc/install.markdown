Title: Getting and Installing Lumen

## Requirements

To build Lumen itself, you'll need an Ada compiler that supports Ada
2005.  One good free compiler is [GNAT][] from AdaCore Technologies.
Yes, that's all!  The demos now use the "streamlined" OpenGL bindings
used by Lumen internally.  The library also includes some thin OpenGL
bindings, though we discourage their direct use; either use the
streamlined bindings (Lumen.GL, GLU, and soon to come, GLExt), or wait
for our fancy "thick" Lumen.OpenGL bindings.

(NOTE: As of this writing, May 2011, Lumen does use one GNAT-specific
library routine, so it is currently dependent on GNAT.  It's a very
trivial task being done, though, and we're working on replacing it
with in-line code.  No reports have come in of anyone trying to build
it with another compiler, though, so it's not a priority.)


## Fetching

A preliminary form of the Lumen library, and some demos for it, can be fetched
using this [git][] command:

        git clone git://ftp.niestu.com/lumen

The repository is now [replicated on github][github], so as an
alternative to the above you can use this:

        git clone https://github.com/karakalo/Lumen.git

That will create a subdirectory named `lumen` (or `Lumen` if you use
the github repo) containing the library, its demos, and its docs.


## Building Lumen

Once you have downloaded the source, these commands will build Lumen itself:

        cd lumen
        gnatmake -P lumen.gpr

That should create `liblumen.a` and a clutter of `.ali` files in the
`lib` directory.  If you want to build the optional joystick support
as well (note that it is Linux-specific), this command should do it:

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

That should create various executables in the `bin` directory, which
you can run according to the instructions on their respective
description pages.

Lumen now also includes the first few lessons from the
[NeHe OpenGL tutorials][nehe], which can be built thusly:

        gnatmake -P nehe.gpr

This should create more executables in the `bin` directory.


## Future

That's all for now, but more is coming soon!


[demos]:   narrative-demos.html
[git]:     http://git-scm.com/
[github]:  https://github.com/karakalo/Lumen
[gnat]:    http://libre.adacore.com/libre/
[holm]:    http://adaopengl.sourceforge.net/
[mesa]:    http://www.mesa3d.org/
[nehe]:    http://nehe.gamedev.net/
[oglada]:  http://www.niestu.com/software/oglada-0.3.tar.bz2
[opengl]:  http://www.opengl.org/
[x.org]:   http://www.x.org/wiki/
