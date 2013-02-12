Title: Getting and Installing Lumen

[TOC]

## Requirements

### X11 Systems

Though it should go without saying, to make use of Lumen you will need a
computer system that supports [OpenGL][].  Nowadays that is *almost* every
computer system out there, but as we have found, not all of them.

To test whether your X11 system supports OpenGL, run this command:

        glxinfo | grep 'OpenGL version'

That should print something like this:

        OpenGL version string: 2.1.2 NVIDIA 304.64

or this:

        OpenGL version string: 4.2.11762 Compatibility Profile Context

If it does not print a version string, then you have some investigating to do.
While you may be able to *build* Lumen and its applications, it is likely that
they won't run properly.

### Win32 Systems

We have been told that "if you are running Windows
98/Me/NT/2000/XP/2003/Vista" then your system supports OpenGL.  As with all
things Microsoft, that may or may not be true.

### The Lumen Library

To build Lumen itself, you'll need an Ada compiler that supports Ada
2005.  One good free compiler is [GNAT][] from AdaCore Technologies.
Yes, that's all!  Lumen is not a binding to an external library; anything it
requires should be part of your system already.

### Demos and Other Applications:  X11

To build the demos, and probably any other application that uses the library,
you'll need the "development" versions of the X11 and OpenGL libraries; on
Debian and presumably Ubuntu, the package names are:

 * `libx11-dev`
 * `libgl1-mesa-dev`
 * `libglu1-mesa-dev`

Analagous packages surely exist on other distributions and platforms.  These
are apparently only needed to create some crucial softlinks, but needed they
are, so be sure to install them if you want to build the demos.  (And trust
us, you *do* want to build the demos!)

### Demos and Other Applications:  Win32

In order to build the demos on Microsoft Windows, you need to have the OpenGL
libraries in your path:

 * `OpenGL32.lib`
 * `Gdi32.lib`

You can acquire these files as part of the [Microsoft Windows SDK][mssdk].
Another option is to build them yourself from OpenGL32.dll and Gdi32.dll
following the [GNAT Users's Guide][gnatug].


## Fetching the Source

Currently you must build Lumen and its demo apps from source.  To do this,
you'll need the [git][] version control application.

### Fetching the Lumen Library Source

The latest version of the Lumen library can be fetched using this [git][] command:

        git clone https://github.com/karakalo/lumen.git

The repository is also replicated for historical reasons at NiEstu, so as an
alternative to the above you can use this:

        git clone git://ftp.niestu.com/lumen

That will create a subdirectory named `lumen` containing the library and its
docs.


### Fetching the Demo Application Source

The demo applications were recently split into their own repository, so to
fetch the demos, use this command:

        git clone https://github.com/karakalo/lumen-demos.git

or this alternative:

        git clone git://ftp.niestu.com/lumen-demos

That will create a subdirectory named `lumen-demos` containing the sources of
the demo apps.  You should create this in a location parallel to the `lumen`
directory, or elsewhere in your filesystem, not under `lumen` itself.


## Building the Library

Once you have fetched the sources, you will no doubt wish to build
applications, either the prepackaged demos, or your own apps.  To do this, you
must first build the library itself.  Make sure you have installed the listed
[requirements](#requirements), then proceed below according to your platform.

### Identifying Your Platform

You need to tell Lumen which operating system you're using. This is done by
setting the GPR scenario variable **OS**. It currently supports three values:

 * `Linux`: Compiles Lumen for an X11-based system
 * `MacOSX`: Compiles Lumen for MacOSX
 * `Windows_NT`: Compiles Lumen for Microsoft Windows

Please substitute **$OS** with the value representing your operating system in
the following commands.  The default is Linux, which in this context means
"X11", so if you're building for X11, you can omit the OS specifier, meaning
you do **not** need to add "`-XOS=Linux`" to the gprbuild commands described
below.

### Building the Lumen Library: X11

To build Lumen itself, do:

		cd /path/where/you/put/lumen
		gprbuild -P lumen

That should create `liblumen.a` and a clutter of `.ali` files in the
`lib` directory.

The `gprbuild` command has some configuration it likes, and if that's not set
up on your system yet, it may give you some static.  The program `gprconfig`
lets you set that up, choosing which compilers and languages to use for
builds.  Pick at least a C compiler and an Ada compiler from its menu, and
save the config, after which the above command *should* work.  You only need
to do this once, as it saves the information in a config file.  And you often
do not need to do it at all: Most Linux distributions of GNAT, for example,
come pre-configured.

### Building the Optional Linux Joystick Support

If you want to build the optional Linux joystick support as well, this command
should do it:

		cd /path/where/you/put/lumen
        gprbuild -P joy

The command should add `lumen-joystick.ali` and `liblumenjoy.a` to the `lib`
directory, which will allow you to build the joystick demos.  And if you have
a joystick or a game pad or something similar, you should be able to actually
*run* them!


### Building the Lumen Library: Win32

For compiling Lumen itself, do:

		cd \path\where\you\put\lumen
		gprbuild -P lumen -XOS=Windows_NT

That should create `liblumen.???` and a clutter of `.ali` files in the `lib`
directory.

## Building the Demos and Other Applications

Lumen includes several working applications to demonstrate its use.  These are
simple, brainless apps, but should work out of the box.  See the section on
[identifying your platform](#identifying-your-platform) for information on the
`$OS` setting used in the commands below.

### Finding the Library

Now that Lumen applications may be located in and built in any arbitrary
directory, you must tell the compiler where to find the Lumen library that you
built above.  To do this, you must set an environment variable; on most Unix
systems, you can do it thusly:

        GPR_PROJECT_PATH=/path/where/you/put/lumen
        export GPR_PROJECT_PATH

On MS-Windows, there is a system configuration dialog that lets you set
environment variables.  On both systems, you need do this only once, before
you start building applications.

### Building the Basic Lumen Demos

Once you have built the library, you should be able to build
[the demo programs][demos] by changing into the `lumen-demos` directory and
running this command:

		cd /path/where/you/put/lumen-demos
        gprbuild -P demos -XOS=$OS

That should create various executables in the `bin` directory, which you can
run according to the instructions on their respective description pages.

### Building the NeHe Demos

Lumen now also includes the first few lessons from the
[NeHe OpenGL tutorials][nehe], which can be built thusly:

		cd /path/where/you/put/lumen-demos
        gprbuild -P nehe XOS=$OS

This should create more executables in the `bin` directory.

### Building the Tutorial Demos

Lumen now also includes the first few tutorials from the
[OpenGL Tutorials][tutes] site, which can be built thusly:

		cd /path/where/you/put/lumen-demos
        gprbuild -P ogltut -XOS=$OS

This should create more executables in the `bin` directory.


## Additional Information

### OpenGL Bindings

The demos now use the "streamlined" OpenGL bindings used by Lumen internally,
declared in `Lumen.GL` and `Lumen.GLU`.  The library also includes some thin
OpenGL bindings, though we discourage their direct use; either use the
streamlined bindings, or wait for our fancy "thick" `Lumen.OpenGL` bindings.

### OpenGL Versions

Lumen currently supports any version of OpenGL.  Work is underway to support
OpenGL ES, which is the variant used for mobile and embedded systems such as
Android.  Once that is available, this documentation will be updated to
describe its use.

The current streamlined bindings are intended to support OpenGL 3.3, though
some calls may be missing.  If you find a call you need that doesn't have a
binding yet, please let us know and we will add it.  Bindings to earlier
versions should be fairly complete.

### Ada Compilers Other Than GNAT

Lumen currently does use one GNAT-specific library routine, so it is dependent
on GNAT.  It's a very trivial task being done, though, and we're working on
replacing it with in-line code.  No reports have come in of anyone trying to
build it with another compiler, though, so it's not a priority.

### Getting Started Using Lumen

See the [Getting Started][start] page for more details about how to begin
using Lumen.

## Future

That's all for now, but more is coming soon!


[demos]:   narrative-demos.html
[git]:     http://git-scm.com/
[gnat]:    http://libre.adacore.com/libre/
[gnatug]:  http://gcc.gnu.org/onlinedocs/gcc-4.1.2/gnat_ugn_unw/Creating-an-Import-Library.html
[mssdk]:   http://msdn.microsoft.com/en-us/windows/bb980924
[nehe]:    http://nehe.gamedev.net/
[oglada]:  http://www.niestu.com/software/oglada-0.3.tar.bz2
[opengl]:  http://www.opengl.org/
[start]:   getting-started.html
[tutes]:   http://www.opengl-tutorial.org/
