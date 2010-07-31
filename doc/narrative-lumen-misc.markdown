Title: Narrative Description of the Lumen Library

This is a simple narrative description of the parts of the Lumen library that
are relatively short, or aren't described elsewhere.

# Contents

* [Lumen](#lumen)
* [Lumen.Binary](#lumen-binary)
* [Lumen.Binary.IO](#lumen-binary-io)
* [Lumen.Internal](#lumen-internal)
* [Lumen.Joystick](#lumen-joystick)
* [Lumen.Window](#lumen-window)

------------------------------------------------------------------------------

# Lumen {#lumen}

The root of the Lumen package family is the empty spec `Lumen`.  Okay, it's
not totally empty--it has a `pragma Pure` in it, for no real reason.

------------------------------------------------------------------------------

# Lumen.Window {#lumen-window}

## Create

The `Lumen.Window` package is sort of the first among peers.  It is where
everything starts, by creating a window.  The `Create` call has a metric
assload of parameters, but they all have defaults, and in most cases the
defaults are at least "usable".  If you just call `Create` with only a `Win`
parameter, you get a 400x400 X window named using the application executable's
filename, with a new double-buffered TrueColor OpenGL rendering context
attached.  Such a window won't receive any input events, and without them
maybe it has limited usefulness, but you certainly can draw in it.

The first parameter is `Win`, which is the place where Lumen puts the handle
for the new window.  The handle is what you use in all other calls that refer
to the window.

The next parameter is `Parent`, which tells which other window this one's
parent is.  If you create more than one window, they can be either top-level
independent windows, by specifying `No_Window` as the parent (which in X
actually uses the screen's root window) , or subordinate children, by
specifying another Lumen window as the parent.  Frankly, we haven't played
with the whole child-window thing yet; in some cases, the child windows are
clipped to the boundaries of the parent.  We're not yet sure if that's true of
all child windows.  But we think there can be some advantages to be had by
declaring a window as a child of another one, though we can't at the moment
state what those advantages might be.  Use this option with caution, because
once we do explore the topic, it may be going away, or changing radically.

The next two parameters, `Width` and `Height` are the new window's initial
dimensions measured in pixels.  Maybe someday we'll allow measurement in
millimeters or furlongs or whatnot, but for now, it's pixels.  You can resize
the window later, although not from within Lumen itself; the resize request
has to come from outside, meaning the window manager, usually because you
dragged the border to a different size.

The next parameter `Events` is the set of input events you want to receive,
things like keypresses, pointer (mouse) movements and button clicks, the
aforementioned resize requests, and so on.  Just set whichever event type(s)
you want to receive to True, and the rest False.  See the
[`Lumen.Events`][events] package for how you actually receive such events in
your code.

The next four parameters are various names to attach to the new window.  The
plain `Name` parameter sets the window's name (also known as the WM_NAME
property), which most window managers will put in the window's title bar or
its equivalent.  The `Icon_Name` is associated with the window's icon rather
than the window itself; a lot of window managers don't even use this one, but
it's there for those that do.  The next two, `Class_Name` and `Instance_Name`,
together collectively make up the window's "class name", the first being the
actual class, and the second being the instance of that class.  Some window
managers, like the one we use (openbox) let you refer to window classes in
their configuration, so you can treat all windows of a certain class the same
way.  If you don't specify a `Class_Name`, it'll initial-cap your app's
filename and use that, so if your app is called "foo", the class name will be
"Foo".  If it's "foo\_bar", the class name will be "Foo\_Bar", and so on.  Why
this is a tuple (two strings used as a unit) instead of just one string, we
have no idea.

The `Context` parameter is in case you want to get fancy and have multiple
OpenGL rendering contexts, and swap them out one for another.  This parameter
lets you create a new native window with an existing context, instead of
creating a new context like it would otherwise.  You can create new contexts
with the `Create_Context` function described below; if there's a reason for
it, we'll add another function like `Get_Context` that will return an existing
window's context so you can share the same context across multiple windows.
You can also set (change) a window's context after it's created using
`Make_Current` as described below, so if you didn't set it at creation, you
can change it later.  We're not yet sure why you'd want to do this, but we'll
explore it soon.  This is another parameter that may change or go away as we
learn more.

The last four parameters, `Depth`, `Direct`, `Animated`, and `Attributes`, are
related to the X windows "visual" and the OpenGL context created for the new
window, and control the kinds of colors you can use, how updates are sent to
the server, and other useful but rather obscure stuff.  The defaults are fine
for most current hardware, but in unusual situations you may need to tinker
with these values to find a set you can use to create a window.

`Depth` in this case is the *color* depth, meaning either a reduced
PseudoColor or DirectColor palette, or a 32-bit RGBA TrueColor palette.  The
term "depth" is also used in the attributes below, with an entirely different
meaning.  The `Depth` parameter allows you to use lesser hardware, or make
less use of regular hardware, by using PseudoColor or IndexedColor rendering
instead of TrueColor.  We've used those in the past, but it's been so long
we've forgotten if they have any advantages besides being able to run on
ancient graphics cards.

`Direct` is an aspect of the OpenGL rendering context, and specifies whether
you want direct rendering, or to go through the X server.  If you don't know
or care what this means, just use the default.  If you get a `Context_Failed`
exception, try setting it to `False`.  If you're sending the display over the
network, you usually have to set this to `False`.

`Animated` says "I'll want to do smooth animation in this window", and allows
that by selecting a double-buffered visual instead of one that's
single-buffered.  If you set it `True`, or let it default to `True`, then
you'll need to call `Swap` to see what you've just drawn.

Finally, `Attributes` is a list of X visual settings that you can use to
determine what sort of graphical "device" you're going to be drawing on.  In
the old days, if you had a graphics terminal with, say, 4 bits for each of
red, green, and blue, well that's what you had.  In today's world, X normally
provides you with several choices of color depth, Z-buffer depth, double
buffering, and so on.  Using the `Atributes` parameter you can pass an array
of various visual attributes; the `Default_Context_Attributes` constant
provides an example of how to do this.

During testing we discovered that not all setups offer 8 bit RGBA visuals with
24 bits of Z buffer, so if you get a `Not_Available` exception from `Create`,
try lowering some of those values, even all the way to zero.  X will pick the
"best fit" if you ask for less than he has to offer.

Another thing to note about the `Attributes` array is that two of its values
overlap two distinct parameters to `Create`:

* Including `Attr_RGBA` is the same as setting `Depth` to `TrueColor`.

* Including `Attr_Doublebuffer` is the same as setting `Animate` to `True`.

These overlaps are due to historical reasons and may go away.  `Animate` will
probably hang around as a separate parameter, since it's a pretty important
aspect of all renderings, but `Depth` may just be dumped in favor of including
`Attr_RGBA` in the default values, since TrueColor is pretty much the norm
these days.

There are many elaborate things that X windows can do, and Lumen elides most
of those.  As the need, and hardware to test on, arises, such elaborations
might be added to the package.  Things like multiple screens, for example.

We expect that this call, and `Swap`, will be the most-used routines in this
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
change either one (or both).  Hey, we didn't make the rules!  And we're too
lazy to write code that fetches the old value and uses it to set the new one.
Mostly because we don't anticipate a lot of demand for on-the-fly window
class-name changes.  If that changes, the rule may change too.

## Create_Context

The `Create_Context` function creates a new OpenGL rendering context, which
may then be set in a Lumen window using the `Make_Current` call.  We're sure
there are reasons to do this, but we're not yet sure what they are.  Another
one to use with caution.  Uses whatever X visual the Win is already using.

## Destroy_Context

The `Destroy_Context` call reverses the action of a `Create_Context` call by
destroying a previously-created OpenGL context.  Why you'd want to do this, we
have no idea.  But if you create something, you should be able to destroy it,
so there you go.  We assume it's a bad idea to destroy a context before you're
done using it, and it's up to you to ensure that you don't do that.  If all
you ever do is call `Create` and draw stuff, you'll never need this call, nor
`Create_Context` nor `Make_Current`.

## Make_Current

The `Make_Current` call makes an OpenGL rendering context the current one for
a given window.  If you use `Create_Context` to create a new context, this is
how you actually use that context.  Another part of the OpenGL processing
chain we haven't fully explored.

## Swap

This is the second most useful routine in the whole package.  It's necessary
if you've created a double-buffered rendering context by setting `Animated` to
`True` in a `Create` call, you'll need to call this each time after you've
drawn stuff, so that it will then be displayed.  Without double buffering,
don't use this call.

We expect that this call, and `Create`, will be the most-used routines in this
package; the others are for much more specialized situations.

------------------------------------------------------------------------------

# Lumen.Joystick {#lumen-joystick}

Lumen joystick support is the ugly stepchild of input facilities, mainly
because very few of the early testers actually have joysticks or gamepads.
The current code works, or at least should work, for the Linux joystick
interface.  Because of its limited popularity, it is entirely separate from
the rest of the Lumen library and can be safely omitted from your builds
without consequence.

But if you *do* have a joystick and want to use it with a Lumen app, step
right up!  The `Open` procedure opens the "first" joystick by default, and
returns a handle to use for future reference to that stick in your code.  You
can provide it with a different device path to select a different stick.  The
`Close` call reverses this action.

Once you've opened a joystick, you can fetch various bits of information about
it, using the `Name`, `Axes`, and `Buttons` functions, whose return values
should be obvious from their names and data types.  Use these to determine
whether the user's device has what your app wants.

The `Pending` and `Next_Event` functions work much like their namesakes in the
[`Lumen.Events`][events] package. `Pending` returns the number of joystick
events waiting for you to read from that stick, and incidentally tells you
whether `Next_Event` will block (wait) if you call it.  (Hint: It will block
if `Pending` returns zero.)  `Next_Event` returns the next joystick event from
that device, either immediately handing back the first one waiting in the
queue, or blocking until a new one shows up.

During development, we tried a technique that would "bind" a joystick to a
Lumen window, and return that joystick's events along with all the normal
window events like mouse moves and button clicks and keypresses, but the
implementation proved too slow and unwieldy.  Such a mechanism might reappear
if a breakthrough, or a miracle, occurs.  For now, you have to handle joystick
events separately from the rest of your input events, using the functions
provided by this package.

------------------------------------------------------------------------------

# Lumen.Binary {#lumen-binary}

This is a little set of declarations that are useful for low-level bit and
byte thwacking.  You're welcome to use them in your own apps if you find them
appealing.

------------------------------------------------------------------------------

# Lumen.Binary.IO {#lumen-binary-io}

A simple package that uses `Ada.Streams.Stream_IO` to implement the reading
and writing of some of the data types in its parent package.

------------------------------------------------------------------------------

# Lumen.Internal {#lumen-internal}

Don't look at this package.  It's not for applications, and exists only to
allow the `Lumen.Window` and `Lumen.Events` packages to share some of the
internal X window binding stuff.

------------------------------------------------------------------------------

[events]: narrative-lumen-events.html
