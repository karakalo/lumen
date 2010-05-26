Title: The Lumen User Interface Library

<h1 class="centered">The Lumen User Interface Library</h1>

Lumen is a user interface library designed to support visually oriented
applications such as games, simulations, and visualization.  It draws
inspiration from, and somewhat resembles, [PUI][], [GLUT][], {GLUI][],
[GlGuiA][], and to some extent, widget libraries such as [Gtk+][], [Qt][], and
[wxWidgets][].

It handles graphical output, including managing windows and providing
"widgets" (graphical program control elements), and user input, from the
keyboard, mouse, and joystick.

Some notable features of Lumen are:

 * It is written in Ada, and provides an Ada API.  Bindings for other
   languages are possible, but the current focus is solely on Ada.  Volunteers
   for binding creation are welcome.

 * It uses OpenGL for its display output.  The one exception to this is its
   provision of standalone popup dialogs using native windowing.  (These are
   separate from the popup dialog widgets provided as part of the standard
   OpenGL widget facilities.)

 * Because of the above points, it is robust, flexible, and highly portable.
   Applications should build "out of the box" and look identical on a wide
   variety of platforms.

 * Standard widget sets are provided for several platforms that are intended
   to emulate the look and behavior of that platform's native widgets, so
   integrating Lumen apps into an existing environment can be made less
   awkward.

 * We have tried to make the process of creating new widget sets as painless
   as possible, by making it simple, clear, and straightforward, by
   documenting it fully, and by providing a tutorial and several examples.
   User-contributed widget sets are welcome, and after they've been checked
   out, will appear in our "contrib" area.

 * It provides both a simplified interface, for creating quick demo or test
   applications, and a more powerful interface for developing sophisticated
   production apps.

 * It is one part of a larger support library, tentatively named "Lace", which
   provides services useful to advanced applications.  Interdependencies are
   minimized, so you don't have to drag in a whale-sized library for a
   mouse-sized program.

Here is a diagram of some of the components of Lumen and how they fit together:

    <div class="centered">
      <img width="890" height="723" alt="[depenencies]" src="lumen.png">
    </div>

Lumen is currently in the very earliest stages of development, so all of the
above is just wishful thinking right now.  But our team is active and growing,
and has significant experience in the subject, so watch this space for
frequent changes.

An incomplete and somewhat rambling [rationale document](rationale.html) is
available, and badly in need of updating.

For more information about Lumen, join the #Ada channel on the [Freenode IRC
network][freenode].

[pui]:       http://plib.sourceforge.net/pui/index.html
[glut]:      http://www.opengl.org/resources/libraries/glut/
[glui]:      http://glui.sourceforge.net/
[glguia]:    http://sourceforge.net/projects/glguia/
[gtk+]:      http://www.gtk.org/
[qt]:        http://trolltech.com/products/qt/
[wxwidgets]: http://www.wxwidgets.org/
[freenode]:  http://freenode.net/
