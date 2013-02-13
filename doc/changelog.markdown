Title: Major Changes in Lumen, 11 February 2013

First post-reorg changelog, 11 Feb 2013.

* The biggest change is that the demo applications have been moved to a separate
repository, named `lumen-demos`.  The old Lumen repository is still available
on github, under the name `old-lumen`.

* New demos have been added:

    * `info` -- Prints information about the OpenGL capabilities onto
          standard output.

    * `attribs` -- Prints and displays the attributes used to select an X11
          visual.

    * A few of the starting tutorials from the
      [OpenGL tutorial web site][ogltut].  These are all aimed at OpenGL 3.3,
      and the Lumen versions can be built with `gprbuild -P ogltut`.

          * `tutorial01` -- Opens a black screen.  Yay.

          * `tutorial02` -- If everything is working right, draws a large red
           triangle on the black screen.  Uses shaders and vertex buffers,
           ooh, fancy.

* Several new OpenGL calls have been added to the `Lumen.GL` streamlined
  bindings, relating to blending, shaders, and vertex buffers.

* Added the `Lumen.Shaders` package, providing convenience procedures to read
  shader source from a file or a string and compile it, and fetch the info
  log, which contains error messages if there were errors.

* Totally rewrote the [Getting and Installing page][install] to (we hope) make
  it easier to follow.

* Added a [Getting Started With Lumen page][start] to help newcomers get their
  toes into the Lumen waters.

* Made numorous bugfixes.

[install]:  install.html
[ogltut]:   http://www.opengl-tutorial.org/
[start]:    getting-started.html

