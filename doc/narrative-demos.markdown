Title: Index to Narrative Descriptions of the Lumen Demo Applications

These are the narratives discussing the various demonstration applications
that are provided with the Lumen library.  Most of the demos are probably
slanted toward the X window system; MS-Windows and Mac users of Lumen, if
there ever are any, may not be familiar with some of the terms.

* **[`colors`][colors]**: An adaptation of the "glXIntro" demo app, showing
    the simplest and most basic way to create a window, draw something in it,
    and handle some limited input events.

* **[`sgi_simple`][sgi_simple]**: A port of the old SGI demo app "simple",
    again very basic, showing a different way to handle input events.

* **[`spinner`][spinner]**: Showing use of the animation package, and a third
    way to do event handlers.

* **[`texture`][texture]**: A variant of `spinner` showing use of the image
    loading package.

* **[`text1`][text1]**: Direct use of the `Lumen.Font.Txf` package,
    which normal apps shouldn't have to do, once the `Label` widget is
    implemented.

* **[`text2`][text2]**: Another demo of the `Lumen.Font.Txf` package,
    this time displaying actual "live" program data.

* **[`multi`][multi]**: Illustration of an app with multiple windows.

* **[`blending`][blending]**: Tool for exploring the OpenGL blending function.

* **[`fire`][fire]**: Neat "fire" effect using a particle system and blending.

And these are the [NeHe demos][nehe] that we currently have:

* **[`lesson01`][nehedemo]**: Simplest possible app, just opens a
    window and clears it to black, then waits for it to be closed,
    either via the window manager, or with a keystroke.  Sort of a
    "hello world" for OpenGL.

* **[`lesson02`][nehedemo]**: Draws a triangle and a square using default color.

* **[`lesson03`][nehedemo]**: Getting fancy now!  Draws the triangle
    and square with non-default colors.

* **[`lesson04`][nehedemo]**: Spins the triangle and square from lesson03.

* **[`lesson05`][nehedemo]**: Going full 3D now: the triangle and
    square from lesson04 become a pyramid and a cube, respectively.
    The cube also spins in two axes.

* **[`lesson06`][nehedemo]**: Textured spinning cube.

* **[`lesson07`][nehedemo]**: Another textured spinning cube, this
    time with keyboard controls.

* **[`lesson08`][nehedemo]**: Spinning cube with a different texture
    and even more keyboard controls.

[blending]:   narrative-blending.html
[colors]:     narrative-colors.html
[fire]:       narrative-fire.html
[multi]:      narrative-multi.html
[nehe]:       http://nehe.gamedev.net/
[nehedemo]:   narrative-nehe.html
[sgi_simple]: narrative-sgi_simple.html
[spinner]:    narrative-spinner.html
[text1]:      narrative-text1.html
[text2]:      narrative-text2.html
[texture]:    narrative-texture.html
