Title: History and Rationale of the Lumen User Interface Library

History and Rationale of the Lumen User Interface Library
=========================================================


Genesis
-------

So a bunch of us were sitting around drinking and tormenting C programmers
when somebody said that one thing Ada really needs is a good native toolkit
for creating graphical user interfaces.  But wait, somebody pointed out, we
have some nice GUI toolkits available to Ada already, such as
[GtkAda](https://libre.adacore.com/GtkAda/) and
[TASH](http://tcladashell.wiki.sourceforge.net/).  Yes, we said, those do
exist, but they're all just bindings to libraries written in other languages,
and tend to force you into their own strange forms.  Besides, Tcl/Tk is rather
weak and slow, and Gtk+ is big and complex, randomly documented, with a steep
learning curve on its best days.

Once we had decided to throw reason to the winds and put history to the torch,
our imaginations really began to soar.  Was there something to be learned from
the mistakes of the past?  Is there a better way to interact with users?  And
thus Lumen was begun.

One note about the language used in this document:  At the time of its
writing, Lumen was in its early design stages.  This document is in fact part
of that design process.  So sometimes we'll talk about it as if it already
exists, and sometimes will talk about it in the future tense, as what it will
be once it's written.  You'll get the idea, we're sure.


Display
-------

When anybody mentions "graphics" the first thing we think of is
[OpenGL](http://www.opengl.org/).  That may seem odd to some people as the
engine for a user interface library, but it makes perfect sense to us.  See,
back in the old days, when most computers were wood-fired and driven by steam,
if you wanted to do graphics on a computer, you had your employer buy a
graphics station, which cost sixty bajillion dollars and took up the same
floor space as a mid-sized sedan.  Later, an upstart company called Silicon
Graphics Inc. started making standalone graphics workstations that were even
better than those old behemoths.  Those SGI workstations used a graphics
language called GL, which curiously enough stood for "graphics language".

GL was *the* graphics display language on SGI workstations; it wasn't an
interface to some lower-level software, it was the direct interface to the
video hardware, something like the VGA driver on MS-DOS, or the Cirrus or
Matrox driver in X.org.  Every pixel on an SGI workstation's screen was lit up
by a GL call of some kind, including menus, windows, mouse cursors, you name
it.  There was no distinction between "native" graphics and GL, since GL was
the native graphics system.

GL was good.  Very good.  So good, in fact, that people started buying SGI
workstations just so they could code their apps to use GL.  As you might
imagine, this didn't sit well with SGI's competitors.  Eventually, for various
political and technical reasons, SGI got together with some other computer
companies like IBM, DEC, and HP, and created a portable standard version of GL
called OpenGL.  What had been GL is now more commonly referred to as "Iris GL"
to distinguish it from OpenGL.  OpenGL has everything that made GL useful and
interesting, and does it in a better and more flexible way.

Because of the state of the hardware and software at that time, OpenGL
appeared on non-SGI systems as a weird exotic way of doing 3D graphics, and
was effectively isolated from the native display systems.  You did a lot of
setup, held your mouth right, and if you were lucky, your application could
open a frame into which it could draw things using OpenGL calls.  That frame
was usually, but not always, inside a native window, and was mostly
well-behaved, but sometimes would act very strangely compared to other
windows.  Like if you switched desktops, sometimes the OpenGL frame wouldn't
go with its enclosing app window, but would linger on the new desktop like an
unwelcome house guest.  People came to think of OpenGL as something you used
with long-handled tongs and thick leather gloves, and only for drawing 3D
objects.

But now it's the 21st century, and OpenGL has gone a long way down the road
since its early days.  Most modern display cards now implement most or all of
it directly in hardware or at least firmware, and it has in many cases become
even faster than the regular native graphics capabilities, especially if
you're doing things like transparency or, of course, drawing 3D objects.  It's
now fast and "native" enough to do things like play videos (mplayer) and
support a window manager (Compiz Fusion).  And we think it's also time for an
OpenGL-based user interface toolkit, not just for 3D apps, but for
general-purpose use.


Controls
--------

So what do we mean when we say "user interface"?  All computer programs need
to be controlled somehow, and one of the currently popular ways to control
them is via a "graphical user interface", or GUI.  These usually appear as a
collection of "widgets", which are things like text labels, pushbuttons,
text-entry blanks, text display and editing panes, adjustable sliders,
pulldown or popup menus, scrollbars, and so forth, usually controlled by a
mouse (or equivalent, like a touchpad, trackball, or "nipple").

Not all widgets are strictly "controls", though.  One thing most widget sets
include is a "canvas", an area into which the program can draw ... things.
Different canvases let you draw different sorts of things, depending on which
library you're using; some offer simple line drawing, some more complex
objects like polygons and arcs.  Most allow you to put bitmaps and/or pixmaps
(black-and-white versus color) into a canvas.  Many also offer an "OpenGL
canvas", which lets you draw in it using the whole range of OpenGL
capabilities, which are very rich indeed.

People being the busy-fingered primates that they are, as soon as you let them
draw a picture, they right away want to manipulate it somehow, moving parts
around, changing its appearance, adding and subtracting parts, using parts of
it to control the app, and so on.  Thus a canvas, which started life as a
simple output widget, eventually morphs into a control of its own, usually
with the help of a metric buttload of code written by the hapless application
developer him/herself.


Widgets
-------

Our normal reaction to such complexity is to flee, to ignore and shun it as
vigorously as possible.  What if, we asked ourselves, all windows were just
"OpenGL canvases", and any control-type widgets you needed could be drawn for
you, and managed for you, directly in those windows?  And maybe any object in
the world could be a control, just by attaching some sort of "handle" to it
and describing what control action it has.

And that could work for pretty much any sort of conventional widget, too.  A
menu or a selection list or a pushbutton is just a flat (or slightly bumpy, in
the fancier widget sets) rectangle with text printed on it, that behaves a
certain way when you move the mouse pointer onto it and click.  In larger
graphical apps that actually draw pictures, or even virtual worlds, it is
often more natural to control things by wrenching around various parts of the
image, rather than using a slider set off to the side in a "native" window.

Also, while the mouse is the current king of analog user input, and the
joystick its prince, that may not be true tomorrow.  Already there are devices
that track eyeball movement, and while the much-hyped "data glove" has had
about as much success as the flying car, you gotta know that someday soon
there will be a cheap, accurate, simple analog input device that is more
natural for humans to use than sliding a plastic box around on a flat surface.
We've spent millions of years grabbing things in our paws and moving them
around in 3D space, and that still comes pretty easy to us.  Can't we get
closer to that nirvana using current hardware, and maybe be a bit more ready
for new hardware once it comes along?  We like to think so.


Case Studies
------------

Enough yakking, time for some concrete show and tell.


###Conventional GUI

First, here's a typical 2D GUI dialog window, this one from the xchat IRC
program, created using the Gtk+ widget library.  It has several types of
widgets, including pushbuttons, checkbuttons, sliders, text entries, a
"spinner" (a numeric value selector), and a navigation tree.

![XChat Preferences](images/xchat-prefs.png)

Nice, nothing spectacular, but it gives us a sort of baseline for what a GUI
means to most people these days.  And in its context, controlling the
configuration of a chat program, it is pretty much exactly what you need.
Anything fancier would be wasted at best, or more likely just annoying.

Given the power of OpenGL graphics, duplicating the above dialog would be
relatively simple.  But there's not much to be gained by simply cloning Gtk+
on top of OpenGL instead of what it currently uses, although we're sure some
people would find it useful.  So go ahead with that if you want, but we're off
in a different direction.  We'll refer back to this dialog shortly, though.


###Simple OpenGL

Let's consider a program that does actual graphics, in this case Lars
Forsberg's [Lunar Lander 2000](http://www.student.nada.kth.se/~f96-lfo/lunar/).

![Lunar Lander](images/lander.png)

It creates an OpenGL window to display its virtual world, and that's pretty
much it.  It is configured on the command line, and takes its input entirely
from the keyboard (via GLUT), so it doesn't need any graphical input controls.
It writes its textual output on the terminal window where you started it,
which works but is ... rather primitive.  One can guess that Lars might have
liked to add some sort of nice display of those values like altitude, speed,
remaining fuel, and so on, but chose not to either because it was too much
work, or because he didn't want to spoil the clean minimalism of the display.
Our guess is the first one: it was way too much work, because he would have
had to create and manage the display elements himself.  It didn't fit within
the boundaries of the project as he envisioned it.

After Lars wrote the original, Peter Hirschberg ported it from FreeBSD to
MS-Windows.  In the process, he did this to it:

![Lunar Lander 2](images/lander2.png)

See, we told you he wanted the data to be displayed that way!  Pretty much
anybody would.  However, that's only five of the sixteen numbers the original
version displayed, because real estate in the OpenGL display window is more
limited than it is in the terminal window.  Such is life, although with a few
more tools at his disposal, one can imagine Peter adding an optional
"Advanced" display or something, with the rest of the data for the real
astronauts in his audience.


###Hybrid: Wrappers

Note also another addition to Peter's version: the menubar at the top, with
its "File" and "Help" choices.  Those aren't part of the OpenGL display
window, but are provided (almost forced upon you, in fact) by MS-Windows.
Let's call that the "wrapper" method, where an OpenGL canvas is surrounded by
native-toolkit widgets that provide application control.

Here's another example of the wrapper style, Robert Ancell's
[glchess](http://live.gnome.org/glChess):

![glchess](images/glchess3d.png)

It has a menubar at the top, a toolbar beneath that, and a set of additional
controls below the OpenGL canvas, all provided by Gtk+.  Very neat and tidy,
but if you look at it objectively, the contrast between the crisp, hard-edged
controls and the softer, more colorful virtual world of the chessboard is
somewhat jarring.

Here's another wrapper example, Matthias Ott's
[dropit](http://www.scenicworld.de/), which has an even greater contrast
between the Motif controls and the OpenGL display:

![dropit](images/dropit.png)

Note that Herr Ott did take the trouble to display some app-related text and
even graphics inside the OpenGL canvas, in the "pane" he set aside on the
right-hand side of the image.


###Hybrid: Popups

Another way of using native widgets is is the form of a pop-up, like a dialog
or a menu.  Here's an app called [DIVE](http://www.sics.se/dive/), which
depicts a virtual world that's so realistic it even has web browsers.  But
when the application needs to specify some URL's, it dips into a native toolkit:

![DIVE](images/dive.png)

Some apps use both a wrapper and pop-ups:

![Copter Commander 2](images/copcmdr1.png)

Some of the clash between the app's display window and its native-GUI add-ons
could probably be reduced by careful selection of colors and/or textures,
which most native toolkits allow.  And there's a lot to be said for using
native widget toolkits: they're already there in your development environment,
they're typically quite powerful, your users are normally familiar with how
they work, and you (the developer) may also be familiar with their use.  Lumen
doesn't prevent you from using a native toolkit or three in your application,
but it does try to offer an alternative, so that maybe you won't need to.



Structure
---------

While it's all very well and good to rag on the interface choices others have
made in their apps (and often quite a bit of fun as well), it's rather
pointless unless we think we have an alternative.  So let's consider all of
this from another angle.

###Widget Panes

Here is a traditional GUI dialog window, our old friend the XChat preferences:

![XChat Preferences](images/xchat-prefs.png)

That's nothing special.  But to an OpenGL programmer, it looks like a textured
rectangle.  Let's look at rectangles for a moment.  Here's one drawn (without
any texture) in an OpenGL window:

![flat rectangle](images/shot1.png)

Wow, let me catch my breath.  We've left the black background in to make the
rectangle more obvious.  Now let's move it around a bit:

![perspective rectangle](images/shot2.png)

Still not rocket science.  Nor is painting it with a snapshot image of the
dialog box:

![perspective dialog](images/shot3.png)

But we hope this illustrates what we said above, that GUI widgets and the
windows that contain them are just flat rectangles.  Or in the case of Gtk+
and many other toolkits with nice sculpted 3D-looking buttons and such,
slightly bumpy rectangles.  In fact, those toolkits simulate 3D by artful
color selection and shading; there's no actual third dimension being created
except in the user's mind.

In Lumen, there *is* a third dimension.  It may not be useful in any given
situation, in which case you don't have to think about it.  But it's there if
you want it.

Let's backtrack a little ... here's our flat rectangle again, except this time
it's been positioned so that it fills the entire window, and now has the
dialog mapped onto it.  Yes, this is drawn using OpenGL, even though it looks
like a normal Gtk+ dialog window:

![flat dialog](images/shot4.png)

Here's a variation, using a couple of unrelated images stolen from the above
screenshots.  This is what one kind of real Lumen app might look like, except
instead of being an OpenGL frame within a native toolkit wrapper, everything
you see is an OpenGL object, including the control panel:

![example window](images/shot5.png)

Further, in Lumen your widget panes don't actually have to be rectangles:

![cylinder pane](images/shot6.png)

Okay, that might not be very useful, but it does give some hints as to where
we're headed.


###Widget Objects

A lot of the initial inspiration for Lumen came from comments (which we can no
longer locate, help appreciated) by Steve Baker about his OpenGL toolkit
library PUI.  Now PUI is a fine library, and we assume that it does everything
Mr. Baker and its other users want it to, but it suffers from the unfortunate
medical condition of being written in C++.  That simply won't do for Ada
programmers, so we set about correcting that deficiency in our world.

So let's look at the canonical PUI app, FlightGear.  It's a wonder to behold,
a miracle of modern open-source technology, and a large part of its greatness
is due to PUI.  Here's the control panel for one of the airplanes from
FlightGear:

![Cessna panel](images/fg1.png)

Now *that's* what an OpenGL program's controls *should* look like!  Yes yes,
FlightGear is most easily operated using a joystick and the keyboard, because
using the depicted panel controls tends to be a bit clumsy.  But the point is
that you *can* control your airplane almost entirely from the rendered control
panel.  And all the instrument readouts are live active displays of the actual
state of your airplane.  (And yes, the HUD is usually nicer and easier, even
though very few Cessna 172's are equipped with them.  Again, not the point.)

And yet, compromise has reared its ugly head even in this nirvana of
application control.  Here's another shot of that same control panel, with a
little bit added:

![Cessna panel hotspots](images/fg2.png)

See all those yellow rectangles?  Those are the actual active areas of each
control.  They often come in pairs, usually for "up" and "down" or "in" and
"out" or "off" and "on" or "left" and "right", whatever the specific control
requires.  Click on the left-hand rectangle at the base of the throttle and
that pulls the throttle out; click on the right-hand rectangle and that pushes
it in.  Left-hand rectangle on the compass offset knob rotates to the left,
right-hand rotates right.  Left-hand of the radio lowers the frequency,
right-hand raises it.  And so on.

That's actually not too bad in practice, although it is a bit of acquired
knowledge that can be fatal if you don't have it.  But it's almost certain
that it simplified the design of the interface enormously.

In Lumen we take a bit more abstract approach.  Our widgets are controlled by
things we call "targets" and "hooks".  A target is a surface that can be
"pushed" by a cursor object to cause a control effect.  A hook is a surface
than can be "pulled" by a cursor object to cause a control effect.  For
example, the surface of a pushbutton would be a target.  Push on it with your
cursor and the button gets pushed, triggering whatever action you've defined
for that button.  The outer surface (and possibly the front surface too) of a
cylindrical knob would be a hook; drag it around the axis and you cause
whatever control action that knob has attached to it.

It sounds simple because it is.  This idea covers pretty much all control
actions you'd want, both in 2D and 3D.  You can create a simple flat rectangle
that has conventional GUI widgets attached to it, like buttons, sliders,
scrollbars, checkboxes, and so on.  Each widget has either a target (buttons
and checkboxes) or a hook (sliders and scrollbars) on it, which allows you to
manipulate it with your cursor object.

Or you can create a complex control panel like the FlightGear one, and attach
your targets and hooks appropriately, to make the controls behave as they
might in a real airplane.  Of course, that may work better in some situations
than in others; with a mouse, for example, you don't have a finger to flip a
switch, so just as with PUI, some compromises may have to be made.  But the
capability is there, and is a generalized one, so you can customize it to the
needs of your application.  If that's too much work, there are predefined
widget object sets that you can use right out of the box, to put together a
usable interface from predefined parts.

Thus, to define a Lumen control widget, you create an OpenGL object using
standard primitives.  You then attach whatever hooks and/or targets you want
to whatever surfaces of the object that will be the most useful in the app.
Then you define what movements of the object cause which application-specific
actions, like toggling a variable or calling a callback routine.  You then use
the widget object just like any other OpenGL object in your virtual world,
except it responds to your touch and provides input.  And if you want it to be
a flat rectangle full of conventional GUI widgets that's bolted across the
bottom of your window, you can certainly have that.


###Readout Objects

So far we have discussed input widgets, but there are also output widgets.
These are somewhat simpler, because they're under the control of the
application instead of the user, but they work in a way analagous to input
widgets: you define an OpenGL object, then attach it to a variable or a
subroutine.  Changes in the variable, or actions by the subroutine, cause the
object to move, or change color or size, in whatever combination your app
needs.  Support is provided for both linear and angular motion in any plane,
as well as any matrix transformation, which allows shear as well as scaling
and translation.

One abstract variant of this concept is the text readout, which will display
Unicode text on whatever surface you define, using whatever font (presumably
TrueType) and color you choose.  That's in addition to the ability to label
widgets with text strings, which is useful for those 2D conventional widgets,
and possibly others.

We're hoping also to provide a package within Lumen that will extrude text
into actual solid objects that you can then manipulate just like any other
object within your virtual world.  In our wilder fantasies, we also imagine
writing those objects out in a form that will allow them to then be turned
into widgets themselves, in such cases where that might be useful.


Summary
-------

So there we go.  Lumen is all things to all people, with a cherry on top.
Okay, no it's not, but it will be, we hope, an alternative way of looking at
user interfaces, especially those in 3D programs.  Once it exists!

We expect Lumen to provide at least these basic things:

 * A simplified interface, so you can create a simple Ada program with a
graphical interface without having to spend half your development time just
reading the toolkit's manual to figure out how it's done.  One can bash out a
quick Tk interface in no time, and the same will be true of Lumen.

 * Full-featured flexibility, for times when you don't just want to toss off a
one-shot quickie app, but instead are wanting to write a serious application
with a professional-looking interface.  In those cases you may actually have
to delve into the manual, but the toolkit should be up to the heavy-lifting
tasks if you want it to be.

 * Canned widget object sets to make your life easier.  At the very least we'd
like to include ones that mimic the appearance and behavior of some of the
more popular GUI toolkits out there, like Gtk+ and Qt.  And yes, eventually
someday Win32 and Cocoa, once some energetic fans of those environments step
up and contribute.  We may also provide Lumen's own set of standard widget
objects, but it remains to be seen whether that will be useful enough to
warrant the effort.

 * A clear and straightforward (and well-documented) way for developers to
define their own widget objects.  That would include specifying the object
itself, in OpenGL terms, which we call the "appearance", and how the object
reacts to various environmental stimuli, such as being touched somehow by a
cursor or other object, which we call its "behavior".

 * Accomodation for future input and output devices as they appear.  Yes,
today we mortals are pretty much limited to 2D movement on a flat screen,
controlled by a mouse, trackball, touchpad, nipple, or joystick.  And Lumen
should help, not hinder, your efforts to provide a good interface to 2D apps
*and* to 3D apps using only those tools.  But once a good 3D pointing device
comes along, we hope that Lumen will allow full natural use of it in a 3D
virtual world.  Same with true 3D displays.

Plus it will be small and fast and modular and all those other good things
that developers demand of their tools.  I mean, c'mon!
