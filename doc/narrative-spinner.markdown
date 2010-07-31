Title: Narrative Description of "spinner" Demo

This is a simple narrative description of the `spinner` application included
with the Lumen library as a demo.

The `spinner` demo has no command-line parameters, so you just invoke it by
typing its name, like `./spinner` if you're sitting in the `demo` directory,
or `demo/spinner` if you're in the `lumen` directory.  When run, `spinner`
creates a black window with a slowly rotating red and yellow square in the
middle.  It should complete one full rotation every 15 seconds, since it
advances the rotation one degree every frame, and should run at 24 frames per
second.  Terminate it by pressing any key, or closing the window.

The `spinner` demo is an enhancement of the [`sgi_simple`][sgi_simple] demo,
intended to show some additional features of Lumen.  It's getting closer to
what a real Lumen app might typically look like, though it's still very basic.

First off, creating a double-buffered rendering context (by allowing the
default `Animated => True` in [`Lumen.Window.Create`][window]) is actually
necessary here in order to get smooth animation.  And as with all d-b
contexts, we call [`Lumen.Window.Swap`][window] after we've finished our
scene, so as to sort of "publish" it to the user.

Event-wise, `spinner` differs from [`sgi_simple`][sgi_simple] by using
[`Lumen.Events.Animate.Select_Event`][animate] instead of the simpler
[`Lumen.Events.Receive_Event`][events].  The main difference is that
`Receive_Events` passes all events to a single handler procedure, whereas
`Select_Events` passes them to different procedures according to the event
type.  So the call to `Select_Events` passes a short table of callbacks, each
of which handles just one kind of event.

Of note here is that I attach the same procedure, `Quit_Handler`, to two
different event types: `Key_Press` and `Close_Window`.  In this case that
makes sense, since both events just mean "quit".  If, however, the app needed
to do something with the event data, like for example look at the actual key
pressed, then it should probably use two different callback procedures for the
two different event types.  Not that it *must*, though, since each callback
still gets the entire event-data record as a parameter, so it could look at
the event type and process each type differently.  But the whole point of
using `Select_Events` is so each procedure gets only one kind of event and
thus doesn't have to waste time asking which kind it is.  I just merged the
two types here because neither requires any complicated processing.

In the paragraphs above, I sorta conflated the event-loop procedures
(specifically `Receive_Events` and `Select_Events`) in
[`Lumen.Events`][events] with those in [`Lumen.Events.Animate`][animate].
That was on purpose, because the identically-named procedures behave pretty
much identically, except the ones in `Animate` also call a draw-frame
procedure, where the ones in `Events` don't.

Much of the rest of the app is similar to `sgi_simple`, with a few additions.
First, instead of drawing a plain white square, it uses OpenGL's built-in
smooth color blending to draw a fancy two-tone square, ooh la la!  Second, it
rotates the square before displaying it, which is a way to show that animation
is actually happening.

And third, of course, is the animation mechanism itself.  When calling the
`Lumen.Events.Animate` version of `Select_Events`, I said "Here, call this
procedure (`New_Frame`) 24 times every second."  And in most cases, that's
just what it does.  If I had asked it to call `New_Frame` eighty bajillion
times a second, the library might not be able to keep up, in which case it'll
call the procedure as often as it can.  If you *want* your draw-a-frame
procedure to be called as frequently as possible, use the constant `Flat_Out`
for the `FPS` value instead of using some arbitrary big number.

When it's called, `New_Frame` updates the square's rotation value, then calls
`Draw` to re-draw it.  Now, why didn't I just put that little rotation-update
code at the top of `Draw` and use that procedure directly as my new-frame
callback?  Because `Draw` gets called in other places, like after an `Exposed`
event.  And I didn't want those calls to update the rotation, which would have
made it "jerky"--sometimes it would happen on schedule, and sometimes it would
happen when the user is fiddling with the window.  To get the smooth stately
rotation it has, I wanted to update the angle *only*  when the new-frame time
rolls around, once every 24th of a second.  That's a good general rule for
frame-based animation: Update your scene once per frame, and re-draw your
scene when you need to, but don't mix the two processes.

[sgi_simple]: narrative-sgi_simple.html
[window]:     narrative-lumen-misc.html#lumen-window
[events]:     narrative-lumen-events.html#lumen-events
[animate]:    narrative-lumen-events.html#lumen-events-animate
