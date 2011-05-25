Title: Narrative Description of "colors" Demo

This is a simple narrative description of the `colors` application included
with the Lumen library as a demo.

The `colors` demo has no command-line parameters, so you just invoke it by
typing its name, like `./colors` if you're sitting in the `bin` directory, or
`bin/colors` if you're in the `lumen` directory.  When run, `colors` creates a
window which rotates through solid red, blue, and green at one-second
intervals.  Terminate it by pressing any key, or closing the window.

The `colors` demo was the very first application written to use the Lumen
library.  It uses low-level calls, and most real Lumen apps won't look much
like it.  But it did help Lumen get up and stumbling with its two most
fundamental services: Opening a window into which OpenGL rendering can be
done, and reporting user input events to the app.  It was adapted from
[the simple `glXIntro` demo program][glxintro] found on the web.

It starts off using `Lumen.Window.Create` to create its window, which creates
a native window and an OpenGL rendering context to go with it.  It sets the
window name (which on my system appears in the window's title bar), asks for
it *not* to be double-buffered (`Animated => False`, we'll explain why in a
bit), and asks to see key-press events sent to the new window.

Having created the window successfully, which it knows because otherwise an
exception would have been raised, it goes into its own very simple
custom-crafted event loop.  Lumen provides some predefined "canned" event-loop
routines, which we assume most programs will use, possibly in their own
separate task, but `colors` isn't that complex.  Plus it also serves as an
example of how to write your own event loop, yay!

The first thing the event loop does is process any and all pending events.
These will be key presses, plus any "unmaskable" events like client messages,
plus structure-change notifications, which Lumen asks for automatically.

The event processing checks for key presses and close-window events, which
cause it to exit the main (outer) event loop and terminate the app.  Key
presses happen when you press any key on the keyboard, even "modifier" keys
like Shift or Control.  The `Close_Window` event is generated when you trigger
the window manager's "Close Window" action, usually by clicking an "X" in the
title bar or something like it, depending on your window manager and its
configuration.

After it has gobbled up and processed all the events in the inner loop, the
app then goes into the actual OpenGL drawing code.  Finally!

Most GUI apps spend much of their time blocked on the event queue, waiting for
an event to show up, which doesn't take any real CPU time.  That's what
Lumen's canned event-loop routines do, too, which is why they'll sometimes be
placed into a separate task, so the rest of the app can go about its business
without waiting for an event to come along.  So-called "event-driven" apps, on
the other hand, are actually designed to wait until an event comes along
before taking any action; it's just up to the needs of your app which
structure you'll use.  This app, for example, could check for events in
between color changes, so when you close the window or hit any key, you
wouldn't have to wait until it cycled through all of the colors before
responding.  But it doesn't; guess that's "left as an exercise for the
reader".

The OpenGL "drawing" part of the demo is dirt simple: It clears the window
successively to red, green, and then blue, waiting one second after each
change.  And that's it.  You were maybe expecting bouncing balls or something?

As mentioned above, we chose to create a single-buffered rendering context, by
setting the `Animated` parameter to the `Create` call to `False`.  This
simplifies the app a tiny bit: If we had asked for a double-buffered context,
we'd have had to call `Lumen.Window.Swap` after every color change in order to
actually see the new color.  That's because in double-buffered contexts,
drawing normally happens in the invisible "back buffer", and that buffer is
only displayed once you swap it to the front.  Any good OpenGL reference can
explain what that's all about, and how it avoids "tearing" and other enemies
of smooth animation, but we don't need it here.

[glxintro]: http://glprogramming.com/blue/ch07.html
