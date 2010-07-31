Title: Narrative Description of the Lumen.Events Packages

This is a simple narrative description of the packages in the `Lumen.Events`
family.

# Contents

* [Lumen.Events](#lumen-events)
* [Lumen.Events.Animate](#lumen-events-animate)

------------------------------------------------------------------------------

# Lumen.Events {#lumen-events}

This is the package that allows your app to receive input events from the
windowing system.  Currently it's probably very heavily influenced by X
windows, and that's just fine with me.

## Event Types

The event types are governed by what the windowing system can and will send to
any app.  Those propagated by Lumen are a small subset of what X can give you,
and others may be added as the need arises.  But the ones that are there allow
a useful set of interactions to take place, we think.

The events `Key_Press`, `Key_Release`, `Button_Press`, `Button_Release`, and
`Pointer_Motion` should be self-explanatory.  The "pointer" is your window
system's "core pointer", usually a mouse, touchpad, trackball, or other
pointing device, and the "buttons" are those buttons associated with that
device.  A "key" is a key on your keyboard.  `Enter_Window` and `Leave_Window`
tell you that the user's cursor has entered/left your Lumen window, and
where.  The `Focus_In` and `Focus_Out` events just tell you when the window
manager has changed the window's "focus", that is, whether it's the active
window or not.

You could say that the rest are more "administrative".  `Exposed` tells you
that part (or even all) of your window was previously covered up by another
window, but now is visible again.  That usually means you should re-draw your
scene, or at least the part of it that was covered up if your app is fortunate
enough to be able to make that distinction.  Lumen has (or will have)
automated exposure handling to allow you to not think about this event.

The `Hidden` event is sort of the opposite of `Exposed`: it means the user can
no longer see your window.  Your application could use this as an indicator
that it no longer needs to stay busy updating the view, until the next
`Exposed` event.

The `Resized` event tells you that the user has requested that the window be
resized, so you might want to have your app adjust itself to the new size
somehow.  You can do this by scaling the scene so it still fits the viewport,
or by drawing more of the scene than before, or whatever suits your needs.

The `Close_Window` event says that the user has asked the window manager to
close the window, usually by clicking the "X" or hitting Alt-F4 or something,
and now the window manager is telling you.  For a single-window app, this
usually means you can terminate; for apps with multiple windows, it just means
that window has been closed, so stop using it and react accordingly.  Ignore
this event and your user will at least be puzzled, if not downright irate.

## Event Data {#event-data}

Some data accompanies each event.  The `X` and `Y` values are the pixel
coordinates *within the window* where the event happened, both starting at
zero, with `X` starting at the leftmost edge of the window, and `Y` starting
at the top edge and increasing toward the bottom edge.  The `Abs_X` and
`Abs_Y` values are the same, except they're measured from the edges of the
entire screen, not the app's window.  The `Modifiers` are the set of things
(keyboard keys and/or pointer buttons) that were pressed and held down when
the event happened.

Not all of the above values are given for every event, obviously.  Some, like
the focus-change and close-window events, have no data besides their presence.
And a few events have extra data to go with them.

The `Key_Code` value that accompanies `Key_Press` and `Key_Release` events is
the raw numeric code for which key was pressed or released; it may not be
directly useful, which is why Lumen provides translation to convert them to
more useful values like Latin-1 and/or Unicode characters.  If you let the
`Translate` parameter for the event routines default to `True`, then the
`Key_Type` and `Key` fields of these event data records will be set.  The
`Key_Type` tells you what kind of value is in `Key`: an ASCII control
character, a "graphic" (meaning, a printable Latin-1 character), a modifier
like Shift, Control, or Alt, a function key, a "special" like Home or Page
Down or one of the numeric keypad keys, or something Lumen didn't recognize.
In all those cases, the `Key` field will be set to the translated key value,
which is (or should be) the same across hardware setups, unlike `Key_Code`.
If you set the `Translate` parameter to `False`, then `Key_Type` will be set
to `Key_Not_Translated`, and `Key` will be undefined.  So don't use it in that
case.

The `Changed` value that accompanies `Button_Press` and `Button_Release`
events tells you which button was pressed or released.

And the `Width` and `Height` values that accompany the `Exposed` event tell
you the width and height of the newly-visible rectangle whose upper-left
corner is at `X`, `Y`.  For a `Resized` event, they tell you the width and
height the window now has after resizing.

For `Exposed` events, a `Count` field is also included.  Because the exposure
might involve several separate rectangular areas, like if you bring the window
out from underneath several other overlapping windows, you might get several
`Exposed` events in a row, each describing a different newly-exposed
rectangle.  The `Count` field tell how many more are coming up after this one,
and will be nonzero except for the very last of these.  To quote the X man
page, "Simple applications that do not want to optimize redisplay by
distinguishing between subareas of its window can just ignore all ... events
with nonzero counts and perform full redisplays on events with zero counts."
It also says that "if `Count` is nonzero, at least that number ... (and
possibly more) follow".  So don't use it as a loop counter, for example.  It's
more of a rough guide.


## Event Handling Routines

These routines allow your app to receive input events, and pass them to your
own event-handling code.  So calling them "event handling routines" is a bit
of a misnomer.

### Pending

The `Pending` function returns the count of events that are waiting in the
event queue.  If you don't pull them out with `Next_Event`, they'll just pile
up there and make a mess.  If you use one of the canned event-loop routines
(`Receive_Events` or `Select_Events`), they'll pull the events out for you, so
you don't want to call `Next_Event` directly if you use one of those.  The
`Pending` function is most useful if you want to "poll" for events, in case
your code has better things to do than hang around waiting for the user to hit
a key or move the mouse.  Usually, though, it's better to wait for events than
to poll for them; if your app has other processing it needs to do while
waiting, use a separate task for it.  This is Ada, after all, so that's easy.

### Next_Event

The `Next_Event` function fetches the next input event from the queue and
passes it back to your app, where you can see what new activities your user
has performed while you were off processing the previous event.  If there are
no events in the queue, it will block (wait) until one shows up.

The `Translate` parameter tells the function whether you want keycodes from
`Key_Press` and `Key_Release` events to be translated into useful values, or
if you just want to use the raw `Key_Code` value directly.  Don't mess with
this unless you know what you're doing.  And if you do, you probably don't.
See [the description above](#event-data) for details about the translation.

You typically use `Next_Event` to create your own "event loop", fetching
events and processing them yourself in a carefully-crafted jewel of
application code.  But you can also use one of the below-described routines
which contain their own event loops.

### Receive_Events

The `Receive_Events` procedure is a simple "canned" event loop, which waits
for events to show up, fetches them, and then passes them to your own
event-handling procedure, which you specify as a parameter.  That procedure
needs to be able to handle any and all events you requested when you created
the window, so it'll probably have a big `case` statement in it somewhere.

The `Translate` parameter has the same function as it does for `Next_Event`.

### Select_Events

The `Select_Events` procedure is another simple "canned" event loop similar to
`Receive_Events`, but with an added twist: Instead of calling a single event
handler routine for every event, by means of a table of callback routines it
can call a different handler for each different event type, thus replacing the
big `case` statement mentioned above.  Any events you don't want to handle can
be set to `No_Callback` in the table.  If you requested those events at window
creation time, they'll simply be ignored; if you didn't request them, then
your app won't even receive them, and it doesn't matter what you put in the
callback table in their slots.

The `Translate` parameter has the same function as it does for `Next_Event`.


## Key translation routines

These are a few simple routines to convert to and from the Lumen data type
`Key_Symbol` and the standard Ada type `Character`.

The `To_Character` routine is only useful for control and graphic characters,
and does the same basic job as the standard `Character'Val` attribute
function, except it will raise the `Not_Character` exception if the key symbol
you give it is not in the range of `Character`.

`To_UTF8` is a bit more worthwhile.  Since Unicode is becoming ever more
widespread these days, more and more situations are just dropping support for
Latin-1, at least for the "hi-bit" characters, those whose ordinal values are
between 128 and 255.  These are things like the accented characters used by
some Western European languages.  If you want to, say, write characters you
get from Lumen keystroke events to a Unicode stream encoded using UTF-8, this
function will give you the encoded characters as a two-character string.  It
also works correctly for "lo-bit" characters 0 .. 127, by simply passing them
through as a one-character string.  Key symbols that aren't in range of the
Latin-1 character set will raise an exception.  Lumen events won't contain any
actual characters that are not Latin-1 (because things like function keys and
Home and so on are not considered "characters"), so `To_UTF8` doesn't handle
those more extended Unicode values.  We're still thinking about the best way
for Lumen to handle full Unicode input; suggestions are welcomed.

`To_Symbol` is just a type-conversion function, for use in those odd
situations where you might need it.

------------------------------------------------------------------------------

# Lumen.Events.Animate {#lumen-events-animate}

This package implements simple frame-based animation.  Or rather, it makes it
easier for you to do so, using your own drawing routine.


## Event-Loop Routines

The `Receive_Events` and `Select_Events` procedures in the `Animate` package
are the direct equivalents of the same-named procedures in `Lumen.Events`,
with one addition: They both accept a `Frame` parameter, which is a pointer to
a procedure that draws one frame of an animated scene.  They also accept an
`FPS` parameter, which is the frame rate you want in frames per second.

You provide the `Frame` procedure, and the Lumen event-loop routines will call
it repeatedly.  If your hardware can handle it, meaning if your drawing code
doesn't take longer than the frame rate to execute, then it will be called
`FPS` times per second.  Your procedure receives a "delta" value, which is the
amount of time that has passed since the last time it was called.  Normally
this will be close to the inverse of the frame rate; that is, if you set `FPS`
to 60, then `Frame_Delta` will be 1/60th of a second.  But if your code takes
longer than the frame rate to draw a single frame, the delta could be bigger.

Note that the very first time your frame procedure is called, the
`Frame_Delta` value is meaningless, or at least not very useful, since there
was no prior frame to measure from.  So it measures from "when time began", or
at least from when `Ada.Calendar.Time` values start, which is January 1st of
the year 1901.  Thus the value you get on that first call will be *very*
large, so can easily be ignored.

These routines should probably always be used with a double-buffered OpenGL
rendering context, meaning you passed `Animated => True` to
[`Lumen.Window.Create`][misc].  Therefore, your `Frame` procedure should call
`Lumen.Window.Swap` once it's done drawing.  You don't *have* to do either of
those things, but you probably won't like the result if you don't.

And what do we mean by "if your drawing code takes longer than the frame rate
to execute"?  Well, let's say you set FPS to 60, meaning "I want my animation
to run at 60 frames per second".  That means your `Frame` procedure had better
do its business and return in less than 1/60th of a second, or just over 16
milliseconds.  With today's hardware, that shouldn't be a problem until your
scene and/or its underlying calculations get pretty darned complex.  And if it
*does* take longer than that, then Lumen will call `Frame` as frequently as
possible, with no delay in between calls.  Your code can tell if it's
exceeding the frame rate by checking the `Frame_Delta` parameter's value.


## Information Routines

You can find out what your actual frame rate is by calling the `FPS`
function.  It can return either of two values:  The overall frame rate since
the application started (`FPS_Overall`) which may or may not be meaningful
depending on your app and its needs, and the frame rate since the last time you
called the `FPS` function (`FPS_Since_Prior`), which may or may not be
meaningful depending on how long it's been since you last called it, and what
your app has been doing in between.

These are both calculated by taking the difference in wall-clock time between
the selected intervals, and dividing by the number of frames rendered in that
interval.  So they can be considered "averages" in some sense of the word.
Suggestions for other kinds of frame rate returns are welcome.

------------------------------------------------------------------------------

[misc]:   narrative-lumen-misc.html
