Title: Narrative Description of the Lumen Library

This is a simple narrative description of the Lumen library.  It is not
intended to be a formal document, so no heckling.  It, and the library as it's
coded now, are probably slanted toward the X window system; MS-Windows and Mac
users of Lumen, if there ever are any, may not be familiar with some of the
terms.

------------------------------------------------------------------------------

# Lumen

The root of the Lumen package family is the empty spec `Lumen`.  Okay, it's
not totally empty--it has a `pragma Pure` in it, for no real reason.

------------------------------------------------------------------------------

# Lumen.Window

## Create

The `Lumen.Window` package is sort of the first among peers.  It is where
everything starts, by creating a window.  The `Create` call has a metric
assload of parameters, but they all have defaults, and in most cases the
defaults are at least "usable".  If you just call `Create` with no parameters,
you get a 400x400 X window named using the application executable's filename,
with a new double-buffered TrueColor OpenGL rendering context attached.  Such
a window won't receive any input events, and without them maybe it has limited
usefulness, but you certainly can draw in it.

The first parameter is `Parent`, which tells which other window this one's
parent is.  If you create more than one window, they can be either top-level
independent windows, by specifying `No_Window` as the parent (which in X
actually uses the screen's root window) , or subordinate children, by
specifying another Lumen window as the parent.  Frankly, I haven't played with
the whole child-window thing yet; in some cases, the child windows are clipped
to the boundaries of the parent.  I'm not yet sure if that's true of all child
windows.  But I think there can be some advantages to be had by declaring a
window as a child of another one, though I can't at the moment state what
those advantages might be.  Use this option with caution, because once I do
explore the topic, it may be going away, or changing radically.

The next two parameters, `Width` and `Height` are the new window's initial
dimensions measured in pixels.  Maybe someday we'll allow measurement in
millimeters or furlongs or whatnot, but for now, it's pixels.  You can resize
the window later, although currently not from within Lumen itself; the resize
request has to come from outside, meaning the window manager, usually because
you dragged the border to a different size.  I may add an explicit "resize"
call to the library if the need seems sufficient.

The next parameter `Events` is the set of input events you want to receive,
things like keypresses, pointer (mouse) movements and button clicks, the
aforementioned resize requests, and so on.  Just set whichever event type(s)
you want to receive to True, and the rest False.  See the `Lumen.Events`
package for how you actually receive such events in your code.

The next four parameters are various names to attach to the new window.  The
plain `Name` parameter sets the window's name (also known as the WM_NAME
property), which most window managers will put in the window's title bar or
its equivalent.  The `Icon_Name` is associated with the window's icon rather
than the window itself; a lot of window managers don't even use this one, but
it's there for those that do.  The next two, `Class_Name` and `Instance_Name`,
together collectively make up the window's "class name", the first being the
actual class, and the second being the instance of that class.  Some window
managers, like the one I use (openbox) let you refer to window classes in
their configuration, so you can treat all windows of a certain class the same
way.  If you don't specify a `Class_Name`, it'll initial-cap your app's
filename and use that, so if your app is called "foo", the class name will be
"Foo".  If it's "foo\_bar", the class name will be "Foo\_Bar", and so on.  Why
this is a tuple (two strings used as a unit) instead of just one string, I
have no idea.

The `Context` parameter is in case you want to get fancy and have multiple
OpenGL rendering contexts, and swap them out one for another.  This parameter
lets you create a new native window with an existing context, instead of
creating a new context like it would otherwise.  You can create new contexts
with the `Create_Context` function described below; if there's a reason for
it, I'll add another function like `Get_Context` that will return an existing
window's context so you can share the same context across multiple windows.
You can also set (change) a window's context after it's created using
`Make_Current` as described below, so if you didn't set it at creation, you
can change it later.  I'm not yet sure why you'd want to do this, but I'll
explore it soon.  This is another parameter that may change or go away as I
learn more.

The last two parameters, `Depth` and `Animated`, are related to the OpenGL
context created for the new window.  `Depth` allows you to use lesser
hardware, or make less use of regular hardware, by using PseudoColor or
IndexedColor rendering instead of TrueColor.  I've used those in the past, but
it's been so long I've forgotten if they have any advantages besides being
able to run on ancient graphics cards.  `Animated` says "I'll want to do
smooth animation in this window", and allows that by creating a
double-buffered context instead of one that's single-buffered.  If you set it
True, or let it default to True, then you'll need to call `Swap` to see what
you've just drawn.

There are many elaborate things that X windows can do, and Lumen elides most
of those.  As the need, and hardware to test on, arises, such elaborations
might be added to the package.  Things like multiple screens, for example.

I expect that this call, and `Swap`, will be the most-used routines in this
package; the others are for much more specialized situations.

## Destroy

This call is here mostly for those fancy-pants who create and manage multiple
windows.  There's no harm in calling it just before your app terminates, but
the window will be destroyed automatically at that point anyway.  This call is
to destroy windows *during* the app's execution.

## Set_Names

The `Set_Names` call is in case you forgot to set the various names attached
to your window correctly when you called `Create`.  Or, well, maybe it's more
useful if you want to *change* any of those names *after* you've created it,
like in response to a change in what's being displayed, or the window's
overall mood, or whatever.  Any parameters not explicitly specified will leave
the corresponding name unchanged, except that since `Class_Name` and
`Instance_Name` are components of a tuple, you have to specify them both to
change either one (or both).  Hey, I didn't make the rules!  And I'm too lazy
to write code that fetches the old value and uses it to set the new one.
Mostly because I don't anticipate a lot of demand for on-the-fly window
class-name changes.  If that changes, the rule may change too.

## Create_Context

The `Create_Context` function creates a new OpenGL rendering context, which
may then be set in a Lumen window using the `Make_Current` call.  I'm sure
there are reasons to do this, but I'm not yet sure what they are.  Another one
to use with caution.  Currently doesn't accept the `Depth` or `Animated`
parameters, which it really should and will soon.

## Destroy_Context

The `Destroy_Context` call reverses the action of a `Create_Context` call by
destroying a previously-created OpenGL context.  Why you'd want to do this, I
have no idea.  But if you create something, you should be able to destroy it,
so there you go.  I assume it's a bad idea to destroy a context before you're
done using it, and it's up to you to ensure that you don't do that.  If all
you ever do is call `Create` and draw stuff, you'll never need this call, nor
`Create_Context` nor `Make_Current`.

## Make_Current

The `Make_Current` call makes an OpenGL rendering context the current one for
a given window.  If you use `Create_Context` to create a new context, this is
how you actually use that context.  Another part of the OpenGL processing
chain I haven't fully explored.

## Swap

This is the second most useful routine in the whole package.  It's necessary
if you've created a double-buffered rendering context by setting `Animated` to
`True` in a `Create` or `Create_Context` call, you'll need to call this each
time after you've drawn stuff, so that it will then be displayed.  Without
double buffering, don't use this call.

I expect that this call, and `Create`, will be the most-used routines in this
package; the others are for much more specialized situations.

------------------------------------------------------------------------------

# Lumen.Internal

Don't look at this package.  It's not for applications, and exists only to
allow the `Lumen.Window` and `Lumen.Events` packages to share some of the
internal X window binding stuff.

------------------------------------------------------------------------------

# Lumen.Events

This is the package that allows your app to receive input events from the
windowing system.  Currently it's probably very heavily influenced by X
windows, and that's just fine with me.

## Event Types

The event types are governed by what the windowing system can and will send to
any app.  Those propagated by Lumen are a small subset of what X can give you,
and others may be added as the need arises.  But the ones that are there allow
a useful set of interactions to take place, I think.

The events `Key_Press`, `Key_Release`, `Button_Press`, `Button_Release`, and
`Pointer_Motion` should be self-explanatory.  The "pointer" is your window
system's "core pointer", usually a mouse, touchpad, trackball, or other
pointing device, and the "buttons" are those buttons associated with that
device.  A "key" is a key on your keyboard.  `Enter_Window` and `Leave_Window`
tell you that the user's cursor has entered/left your Lumen window, and
where.  The `Focus_In` and `Focus_Out` events just tell you when the window
manager has changed the window's "focus", that is, whether it's the active
window or not.

The rest are more "administrative", I guess you could say.  `Exposed` tells
you that part (or even all) of your window was previously covered up by
another window, but now is visible again.  That usually means you should
re-draw your scene, or at least the part of it that was covered up if your app
is fortunate enough to be able to make that distinction.  Lumen has (or will
have) automated exposure handling to allow you to not think about this event.

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

One note: Currently the X server has no useful way of returning joystick
events, so those don't appear in the `Lumen.Events` package.  Depending on how
things shape up, they may show up as new event types, or I may put them in a
separate `Lumen.Joystick` package, dunno yet.

## Event Data

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

The `Key_Code` value that accompanies `Key_Press` and `Key_Release` events is the
raw numeric code for which key was pressed or released; it may not be directly
useful, which is why Lumen provides (or at least, will provide, as soon as I
can write them) translation routines to convert them to more useful values
like ASCII and/or Unicode characters.

The `Changed` value that accompanies `Button_Press` and `Button_Release`
events tells you which button was pressed or released.

And the `Width` and `Height` values that accompany the `Exposed` event tell
you the width and height of the newly-visible rectangle whose upper-left
corner is at `X`, `Y`.  For a `Resized` event, they tell you the width and
height the window now has after resizing.

## Event Handling Routines

These routines allow your app to receive input events, and pass them to your
own event-handling code.  So calling them "event handling routines" is a bit
of a misnomer.

### Pending

The `Pending` function returns the count of events that are waiting in the
event queue.  If you don't pull them out with `Next_Event`, they'll just pile
up there and make a mess.  This function is most useful if you want to "poll"
for events, in case your code has better things to do than hang around waiting
for the user to hit a key or move the mouse.  Usually, though, it's better to
wait for events than to poll for them; if your app has other processing it
needs to do while waiting, use a separate task for it.  This is Ada, after
all, so that's easy.

### Next_Event

The `Next_Event` function fetches the next input event from the queue and
passes it back to your app, where you can see what new activities your user
has performed while you were off processing the previous event.  If there are
no events in the queue, it will block (wait) until one shows up.

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
