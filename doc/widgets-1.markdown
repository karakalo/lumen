Title: Lumen Widgets: First Ramble

<h1 class="centered">Lumen Widgets: First Ramble</h1>

# Contents

* [Introduction](#intro)
* [Current](#current)
* [Layout](#layout)
* [Events](#events)
* [Objects](#objects)
* [Background Info](#background)
* [Responses](#responses)

---

## Introduction {#intro}

Okay, here's the deal.  I've been wrestling with the notion of widgets in
Lumen for a long time now, because I want them not only to not suck, but also
to possibly be great, excellent, elegant, all those things that are possible
when starting a new design from scratch, with no legacy to adhere to.  We have
the opportunity here to do something new and different, not different just
for its own sake, but *better*.  A lofty goal, yes, but without lofty goals we
stay in the mud.

Below I've tried to gather into one place the stray random thoughts I've had
about widgets.  They're not really organized, which is why I call this a
ramble, but I'm hoping you guys can help pick out any good ideas, add your
own, and put them into some semblance of order so we can actually code
something from them.

---

## Current {#current}

The current state of Lumen widget work could best be described as "stalled".
I've produced one woefully incomplete [scrap of code][scrap] and a lot of
fruitless skull sweat.  Because I'm at the very beginning of this part of the
library's design, I'm sorta overwhelmed by all the possibilities and can't
seem to focus on what needs to be done *next*.

My intent is to produce the simplest of all widgets, the Label, just a string
of text that you can use to label or annotate things in your display, either
attached to objects in your scene, or separate in some "control area".
Obviously a label needs a text string to display, a size, rotation, and
position, foreground and background colors (which can be "transparent"), and a
place in the widget world (usually called a "hierarchy" in most other
libraries) where it lives, which is (mostly) separate from where on the screen
it lives.

The code snippet shows that I have some ideas about some of those things.  The
parent/sibling/child pointers allow the widgets to be structured into an N-ary
tree, which is I think how most widget toolkits do it.  If there's a better
way, I'd love to use that instead.  My memory of widget hierarchies is that
they're great for some things (eg. destroy a widget and all its children go
away too), and awful for some other things; living within their structure has
caused me some amount of pain, though I'm not sure how much of that is
avoidable.

The other attributes I listed for a Label widget (color, position, text, etc.)
are not represented in the base type, because not all widgets will have those;
they'll presumably be included in the Label derived type.  And labels
sometimes want a foreground and/or background *image*, not simply a color.
And labels have no "behavior", in that they don't react to any sort of widget
events like "select".  That's what buttons do, not labels, which is why I
assume Label will be the simplest to implement.

Another note about the code snippet: I'd initially assumed that the command,
event-handler, and display routines would actually be *lists* of routines
(which I've been calling "chains") that each get called in turn when
processing that activity.  A pre-defined widget would start with a pre-defined
chain of routines, and the coder could add his own routines to the front or
back of the chain (or even somewhere in the middle, I guess) so it gets called
during normal widget operations.  New custom widgets could use existing
routines, and/or custom ones written specifically for that new widget.  But I
can't for the life of me envision exactly how such a chain would be used, so I
made them simple pointers.  I'll switch them to chains the instant someone
offers a plausible use case for them.  I'd assumed I'd use one of the
Ada.Container types for such lists; I've checked their overhead and while it's
not insignificant, I think it's bearable.  But maybe a simpler list mechanism
would do.

Another question I still have is when and where those support routines get
called.  Okay, the event handlers get called when a widget event occurs that
relates to that widget.  (See a [longer discussion about events](#events)
below.)  And the command chain is a voluntary thing, called whenever the app
wants to change something about the widget.  The display chain will get called
when the widget is created (or at least when it's "shown"--different libraries
have different mechanisms for this), but would also need to get called any
time the widget got reconfigured, and also and most importantly, any time the
scene got updated.

It seems onerous to force the developer to code an "update the widgets" (or
even worse, "update *this* widget" for every widget) in his "draw" routine,
though I think it would be okay if the app is handling window events itself.
The real question becomes, how and when do the mainloop routines call the
widget display chains?  Not for every event, surely, nor even every trip
through the loop.  But part of the point of widgets is that they re-display
themselves when they need to without you having to do much, or anything, to
make that happen.

---

## Layout {#layout}

Now we get to placing the widget.  OpenGL objects have a translation, a
rotation, and a scaling.  It'd be a pain to have to separately manage those
for each widget you use, and normally you don't need to: most widget toolkits
offer some sort of layout options which control position and size, and most
don't care about nor implement rotation.  I had a discussion on IRC with Felix
in which he described the layout mechanism used by the various toolkit
libraries.  He said, "An interesting observation is that open source toolkits
are often layout-managed (Gtk+, Qt) while pure commercial toolkits are usually
anchored (Win32, WPF, Cocoa)."

Having not used Win32 in years, and even then not very heavily, and never used
those other two, I don't know what "anchored" means in this context.  I'm used
to "layout managers", I guess.  I especially like the layout options offered
by Tk, which are "pack" (stuff things into their layout area where they'll
fit, subject to a "gravity" that makes them want to stick to a given edge or
corner of the layout area), "place" (where you say "put this widget *here* and
give it an X, Y coordinate pair), "grid" (where you divide the layout area
into rows and columns, and designate an R, C pair when adding a widget), and
"form" (which is kind of a combination of all of the above).  Most other
toolkits I've used offer some variation of those, but I think Tk's is the
easiest to understand and use.

That's all very well and good, and I'd be happy enough if Lumen offered just
the same options that Tk does, but ... some troubling questions arise.  Like,
what is "anchored" geometry management, and should Lumen use it instead of
"layout"?  And since Lumen is dealing with a 3D world instead of the 2D world
that other toolkits use (even GLUT and PUI basically ignore the third
dimension as I recall, using screen coordinates to do 2D layout for their
widgets, which are also all 2D objects), how to do layout in that world?

First we need to decide how and when Lumen widgets will be and use 2D, and
how/when 3D.  In many cases you won't care about, nor want to deal with, the
third dimension, at least in relation to your widgets, either because your app
is doing a strictly 2D display, or because your widgets are just 2D controls
that live somewhere on the screen and are otherwise distinct from the 3D scene
being rendered.  And that's fine, I want that option, and I assume it will get
used a lot in Lumen apps.  For those cases, some form of the Tk layout
options, or something like them, seems appropriate.

But I don't want that to be the *only* option.  I want to be able to put
widgets into the scene, like labels that are attached to objects and travel
with them.  Like objects that are themselves widgets (see [Objects](#objects)
below).  If you are, say, writing a game where you stack one box on top of
another to get something done, it probably wouldn't be useful to designate
either box as being a widget.  But a knob on a door?  Making that a widget
seems like it would be good.  Levers, switches, knobs, dials, buttons, and
other physical objects within the scene are often susceptible to providing
useful functionality if turned into widgets.  And in many cases inserting
pre-defined widgets, like labels, buttons, sliders, text areas, etc., into the
scene would make things nicer too.

Clearly "layout" takes on a whole new meaning in such cases.  You could, I
assume, define "layout volumes" that correspond to the 2D layout areas used by
things like Tk, but are rectangular volumes instead, and which would offer
correspondingly more choices in where and how to place things.  (Obviously a
2D "layout area" is just a 3D layout volume with a thickness of zero.)  But in
many cases that wouldn't be comfortable, I think.  Take the doorknob as an
example:  You want it to stick to the door and move when and where the door
moves.  Even if you had a layout volume to place it in, you'd want *that* to
move along with the door.  So we need some way of "attaching" either
individual widgets, or the layout volumes they're placed into, to objects in
the scene.  Sounds tricky!

That seems to lead to a point where Lumen has to be aware of, and in some ways
participate in, your scene and the virtual world in which it lives.  I guess
that's unavoidable to some extent, but I sure don't want to have to "register"
every scene object with the UI library.  I think it would work to have a
procedure that said something like "call this to reposition this widget", and
call it in the door-moving routine to reposition the doorknob.  Lumen doesn't
need to know about the door, but the door would need to know about the knob,
at least as an abstract thing.  Similar "command" routines could change a
widget's size, appearance, contents, and behaviors based on the app's needs.

---

## Events {#events}

I wasn't entirely accurate above when I said I didn't know what to work on
next.  I am working on something right now, and that's trying to figure out
how widget events should work.

The concept of "event" in the existing Lumen code refers to low-level
window-system events, currently only those provided by X11, and not even all
of those--the current code ignores a lot of events that it may care about in
some future revision.  Currently it knows about things like "key was pressed",
"mouse button was released", "the pointer moved", "the window has gained
focus", and so on.  Those are great, and allow the writing of substantial
applications, but they are not "events" that are meaningful to widgets.

Those are (or will be) more abstract things, like "widget was selected",
"widget (or some part of it) was moved", "pointer is near/on top of widget",
and so on.  They will have to take into account the widget's screen location
and dimensions, and use the low-level window-system events, and translate
those into "widget events" which are then passed to the widget code.

The current Lumen event system allows the app to receive events directly, or
to attach callback routines which are called by one of the mainloop
procedures, `Receive_Events` or `Select_Events`, either the "plain" or
animation versions.  I suppose it would be relatively straightforward to offer
some sort of event-translation call for widget events, and let the app call it
somewhere appropriate in its own event-handling code.  I'm more concerned
about the mainloop procedures.  Right now, the developer gives them a set of
callbacks and sits back and waits for those to be called as window-system
events occur.  Once he creates widgets, though, somehow those mainloop
procedures will have to start intercepting the low-level events, translating
them in widget terms, and passing them to the widgets when necessary.  But I
don't want to deny the app access to the low-level events just because it
created some widgets.

Not entirely sure how that should be done.  Nor *when*:  I'd intended to use a
"ray shooting" algorithm to determine when and whether a low-level event
intersects a widget's bounding volume in some meaningful way.  But for things
like "hover" (meaning, the pointer is "over" a widget on the screen) I can't
see calling the ray shooting code every time we receive a pointer-motion
event.  Given infinite processor power, maybe, but until we have that, not a
good idea, I think.  Calling it for button presses, okay, that'll work.
Calling it for key presses (or as most libraries actually do, key releases),
that's situational:  Don't really want to do it every time for every keyboard
event, but in apps that use a keypress to select a widget, for example, you'd
want to be able to activate it somehow.

So how and when to do events, that's still up in the air.  Suggestions and
brainstorming are invited.

---

## Objects {#objects}

As mentioned in [the rationale][rat], most of the existing widget libraries,
like the ones I list in the [Background](#background) section, offer OpenGL
"canvas" widgets, into which you can draw using those alien and mysterious
OpenGL commands.  What happens inside the canvas is totally separate from what
happens in the world of widgets outside the canvas.  Any connection between
widget activity and activity inside the OpenGL canvas must be provided by
custom code you write.  Which is fine as far as it goes, but it does leave a
certain something lacking in your app's interface.

With GLUT and PUI, and with all other OpenGL-based toolkits I know about, your
widgets are drawn "inside" your OpenGL canvas, or rather, your OpenGL "canvas"
is the only drawing area, so widgets naturally go there.  That's an
improvement, I think, over the "external" widget sets.  If you want to have
your OpenGL scene *here* and your widgets *there*, you can easily segregate
them yourself; overlaying widgets atop the scene is equally easy.  But as far
as I recall, neither GLUT nor PUI make your widgets in any way be objects
inside your OpenGL world.  They're just these alien 2D things that appear
wherever you tell them to, and which are created and manipulated entirely
separately from your scene.

And I guess that's the way people like it, because all such libs work that
way.  Maybe that's so because it's too hard, or unnecessary, to in any way
integrate your widgets into the rest of your scene.  But I'd imagined that
Lumen widgets would (or at least could) be first-class objects.  Not that
you'd need to spend as much time and energy managing them as you would your
regular scene objects, and they could be exempted from things like lighting,
for example (though they could also be included in that if you wanted them to,
if it made the scene look better), but you *could* if you wanted.

Conversely, I'd love to be able to allow the developer to create an object and
then attach "widgetness" to it.  That is, allow it to be placed using the
standard widget layout mechanism (but do not *require* that), and have it
react to widget events in whatever way the developer deemed appropriate.  If a
set of existing widget behaviors work for that object, then they could be
re-used; otherwise the developer would need to code some custom behavior for
the object, using the standard widget framework when appropriate.

See the talk about hooks and targets in [the rationale][rat].  "Widgetness" is
a set of *behaviors*, and those are separate from the *representation*.  While
Lumen will provide a set of pre-defined widgets, both behavior and
representation, it should allow the developer to substitute one or both
portions with his own code or OpenGL object.  Maybe this is crazy or
impossible, but you'll have to convince me it is.

So you could, say, draw a lever on a control panel in your scene, and turn it
into a widget that controlled some aspect of the app's operation.  I assume
that such custom widgets would have to specify one or more bounding volumes
that the event system would use to determine if the user were interacting with
those objects; presumably simple spheres and rectangular volumes would
suffice, maybe cylinders too.

---

## Background Info {#background}

The other widget libraries I've used are okay as far as they go, but they all
seem awkward to use, overly complex, with arcane handshakes and secret
passages, and yet with a surprising lack of flexibility when it really counts.
I want Lumen widgets to be better than that, easier to use, more transparent
(in the development sense, not necessarily visually), more flexible and
tweakable, with more extensive and complete examples of usage and use cases.
Of course Lumen widgets will *resemble* existing widgets, because first, the
others didn't get everything wrong, and second, because that's what "widget"
means to me, and I assume to the other collaborators.

I've used the following widget libraries, that I can remember.  There are
probably others that I'm forgetting, which may be a good thing.

 * Tcl's [Tk][tk], which I've used with Tcl (a truly wretched language to
   write in), Perl (a huge advance over Tcl), Python (even better than Perl),
   and even Ada, (via TASH).  It's a model of simplicity and ease of use,
   which are definitely things I want Lumen to have.  It's somewhat limited
   functionally, though, and from the very beginning tended to be butt-ugly.

 * [Gtk+][gtk], which I used back when it was just "Gtk" and was Spencer and
   Peter's answer to hating Motif.  Used it with Python and Ada, as well as C
   in the old days.  It's a full-bodied complete capable widget set, used in
   many world-class applications, and can be made to look quite pretty.  But
   it's hugely complex, with a lot of historical quirks and oddities, not as
   well documented as it needs to be, and has always seemed a bit stiff and
   hidebound to me.

 * [Qt][qt], a C++ toolkit with a nice look and a simple event model.  I tried
   it when it first came out, but couldn't stand using C++, so stopped
   thinking about it.  There's a very nice Ada binding for it which I have yet
   to try, and supposedly several Python bindings, which I also haven't
   tried.  I don't have much to say about Qt except that I find it "a bit
   weird", both in look and feel.  Can't define it further than that.

 * [FLTK][fltk], the Fast Light Toolkit, a small toolkit with a storied
   history.  It's related to the XForms toolkit, and like that one was derived
   from the Forms library which was written in/for Iris GL, the precursor to
   OpenGL.  XForms used to be binary-only, but is now open source; FLTK was
   always FOSS.  Like Qt, it's a C++ library, and it's oriented heavily toward
   3D and OpenGL but as far as I can tell, doesn't actually use it for
   drawing.  It's a very minimalist library, with a plain-vanilla look, and I
   only mention it here because it's small and light, something I want for
   Lumen too.  Or rather, I want Lumen to be modular enough so you can make
   small and light apps with it, while still offering a full range of complex
   services when you need them.

 * [wxWidgets][wx], another C++ library, originally a Scottish uni project
   whose main purpose was to allow cross-platform GUI apps that would run
   unchanged on Unix/X11 and MS-Windows.  I was in love with it for about
   three weeks, but again ... C++.  Still, it's a nice library.  I'm hoping
   Lumen will be equally cross-platform, once someone steps up and converts
   the window-system-dependent parts to their platform.  It's really not that
   much code, honest--just a few hundred lines.  We'll probably have to
   negotiate some compromises about window-system concepts like events and
   window attributes and whatnot, but I'm sure something can be worked out.
   If you're at all interested, *please* come to the #Ada IRC channel on
   Freenode and speak up!  Or submit an [issue on github][issues] if IRC isn't
   your bag.

 * [GLUT][glut], the uber-toolkit for OpenGL apps.  While all the above
   libraries *support* OpenGL, they do it by offering an "OpenGL drawing
   canvas", a widget you can create and use along with the other widgets, but
   which is a black box whose contents are a mysterious graphical wilderness
   and of no concern to the rest of the library.  GLUT, however, *is* OpenGL,
   doing all its drawing with it, having no life outside the OpenGL world.
   Yes, it's truly ugly, even homelier than the old Xaw toolkit for X11, but
   it works and is normally available for all OpenGL environments.  It has no
   dependencies beyond OpenGL itself, which is a huge selling point and it was
   part of the inspiration for Lumen.

 * [PUI][pui], Steve Baker's Picoscopic User Interface, a C++ library inspired
   by an earlier C library called MUI, written by a guy from SGI, and both do
   their drawing using OpenGL.  It is sort of GLUT on steroids, a "real"
   toolkit with lots of widgets, far beyond GLUT's basic set, and was a large
   part of the inspiration for Lumen.  I've never actually written anything
   using PUI, but I did dabble in MUI and have looked hard at the PUI docs.
   Like so many other OpenGL add-on projects, it seems to be a sort of "get it
   working and screw the details"; it's rather plain-looking, though not as
   ugly as GLUT, and it seems to offer a nice assortment of widgets.  It's
   used in the FlightGear flight simulator app, and seems to serve quite well
   there.

I didn't list other, older toolkits like Xaw, Motif, or Win32, all of which
I've used, but only briefly.  They tend to be of a pattern with the above
list, although Win32 seemed a clear departure in a lot of ways.

So there you have it, my incoherent ramblings.  Time to come to #Ada and start
chatting!

---

## Responses {#responses}

### Sat 17 Dec 2011 03:39:27 MST

[First response][flyx], from Felix, has arrived!  He explains the concept of
"anchored" layout managers, and now I think I understand.  I believe Tk's
"pack" and "form" managers offer that, though not in the exact way he
describes.  It sounds like a useful model, though, and probably something
Lumen will offer too.

He says, "I personally think widgets do not belong to a 3D world ..."
Obviously, from reading the above, I don't agree.  I *do* "have a broader
vision of what a widget is", i think, but even then, just "normal" widgets (2D
widgets specifically) will need to live in a 3D world *somehow*, if the app
creates 3D displays.  Now, that "somehow" offers a couple of choices:

 * You can have one or more 2D "control panels" where your widgets live,
   either set off to the side, out of the way of the scene, or overlaid on top
   of it, either permanently or transiently like a pop-up.

 * You can integrate such a 2D control panel into your scene as an actual
   object, like a physical control panel at which you "sit", or just painted
   on a wall or something.  Or just put it "in" the scene, so it lives in one
   particular place but is not associated with any particular object in the
   scene.

I do want developers to be able to sequester their widgets on the display if
they want, keeping the "controls" here and the "world" there.  That's a fine
way of doing things, and one that should always be available.  It just should
not be the *only* way of doing things.

I imagine many, even most, apps will have a mixture of 2D controls that are on
the screen at all times, or that pop up when requested, and also some 3D
controls that live within the scene.  Like, say, for loading/saving files: Not
much need for 3D there, so a file-save dialog will probably just be a plain
old 2D dialog, a pane with a collection of 2D widgets in it, almost surely a
transient one that appears when requested and is hidden the rest of the time.

Although you *could* make use of 3D even there.  I know of at least one or two
3D file managers, where you fly through your filesystem like flying over a
city (or through a sewer), and drag files around like physical objects.  Cute,
innovative, but maybe not the sort of thing I'D normally prefer for an app's
interaction with the filesystem.

He also says, "The main loop is then hidden in the toolkit's framework. ...  I
personally would give it a try."  Yeah, Lumen already has mainloop procedures,
you should check it out sometime. ;)  It doesn't force their use, but offers
them for use when appropriate.

Also, "I guess it's clear that we really do want to have a scene graph
structure at the OpenGL side to display the widgets."  Okay, this makes
sense.  I'd need to see something more concrete before implementing it, but it
sounds like a promising idea.

"You should think about whether you want your API to be able to register
multiple handlers for one event ..."  Yes, I definitely want that.  That was
part of my thinking behind making "callback chains" for the widgets, where any
number of routines could be appended or prepended to the chain for any given
widget, either as part of making it into a new widget type, or to modify one
single widget's behavior and/or appearance.

One thing I didn't make clear in the [events discussion above](#events) is
that, even if/when the mainloop procedures intercept and translate low-level
events as they come from the window system, in order to pass widget events
along to widgets, those low-level events should still be available to the app
if it wants them.  Whether it should pass along *all* such events, or only the
ones that were not translated and passed to widgets, I'm not sure about.
Maybe let the app choose?

Overall a very nice response, and I thank Felix for it.  You all should
[read it][flyx] yourself and respond with your own thoughts!

[fltk]:  http://www.fltk.org/
[flyx]:  http://wrttn.in/80c7c2
[glut]:  http://www.opengl.org/resources/libraries/glut/
[gtk]:   http://www.gtk.org/
[issues]:    https://github.com/karakalo/lumen/issues
[pui]:   http://plib.sourceforge.net/pui/
[qt]:    http://qt.nokia.com/
[rat]:   rationale.html
[scrap]: lumen-widget.ads
[tk]:    http://www.tcl.tk/
[wx]:    http://wxwidgets.org/
