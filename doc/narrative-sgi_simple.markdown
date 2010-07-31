Title: Narrative Description of "sgi_simple" Demo

This is a simple narrative description of the `sgi_simple` application
included with the Lumen library as a demo.

The `sgi_simple` demo has no command-line parameters, so you invoke it by
typing its name, like `./sgi_simple` if you're sitting in the `demo`
directory, or `demo/sgi_simple` if you're in the `lumen` directory.  When run,
`sgi_simple` creates a black window with a white square in the middle, which
should be about half the width of the window itself.  Terminate it by pressing
any key, or closing the window.

The `sgi_simple` demo is a direct port of a C program called "simple", which
was included as an example in early editions of the famous *OpenGL Programming
Guide*, also known as the "Red Book" because of its mostly-red cover.  This
contrasted it with the "Blue Book", which is *The OpenGL Reference Manual*.
The original "simple" program used ancient SGI infrastructure libraries known
as "tk" (no relation to Tcl/Tk) and "aux".  In our version, those services are
provided by Lumen.

The app uses low-level calls, and most real Lumen apps won't look much like
it.  But it does demonstrate a couple aspects of the library not shown in the
simpler demo [`colors`][colors].

First, unlike `colors` it creates a double-buffered OpenGL context, by letting
the `Animated` parameter to the [`Lumen.Window.Create`][window] call default
to `True`.  That's just for demo purposes, because the app doesn't do any
actual animation; also, the original SGI app did it, so who are we to mess
with tradition?  Having a double-buffered context, it then must call
[`Lumen.Window.Swap`][window], which it does after drawing its scene, at the
end of the `Draw` procedure.

The second major difference from `colors` is that since it draws an actual
object rather than just clearing the window to a solid color, it sets up the
viewport parameters, in the procedure `Set_View`.  Here it uses a simple 2D
orthographic projection, which is the same projection used by most
three-year-olds when creating refrigerator art.

And that works great for this app too.  When you run it, if you resize the
window you'll notice that the attractive white square that it draws is always
square and never becomes oblong.  More about that below.

The third difference is that `sgi_simple` uses one of the "canned" event-loop
routines, [`Lumen.Events.Receive_Events`][events], instead of having a
hand-crafted event loop of its own.  `Receive_Events` passes all events to a
single procedure, in this case one called `Handler`.  That routine checks for
keypresses and close-window events, in which case it raises an exception which
terminates the app.  It also checks for `Resized` events, meaning the user
changed the size of the window.

Because changing the size can also mean changing the *shape*, from its initial
perfect square to any sort of rectangular shape, the event handler routine
gets the new window dimensions from the event data and passes them to
`Set_View`, who uses them to recalculate the viewport settings.  That's how
the square it draws remains square, even if you make the window wide and
short, or tall and narrow.

Finally, the event handler routine re-draws the scene, because one of the
other events it can get is `Exposed`, which means the window was covered up
(partially or entirely) and is now visible again, so now must be re-drawn.
Note that it also re-draws when it gets `Resized` events, after the viewport
has been reset.

One final note about this app: Note that in the call to
[`Lumen.Events.Receive_Events`][events] it uses the Ada attribute
`Unrestricted_Access` to get a pointer to the `Handler` callback procedure.
This is a non-standard attribute specific to the GNAT compiler, and is not to
be used in polite society.  Normal Lumen apps would use the standard `Access`
attribute, and would declare their callbacks at the library level, meaning in
a package.  I used the non-standard attribute here to avoid having a separate
package just for the one lone callback, so just for simplicity's and
expediency's sake.

[colors]: narrative-colors.html
[window]: narrative-lumen-misc.html#lumen-window
[events]: narrative-lumen-events.html#lumen-events
