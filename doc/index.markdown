Title: The Lumen User Interface Library

<h1 class="centered">The Lumen User Interface Library</h1>

Lumen is a user interface library for Ada designed to support visually oriented
applications such as games, simulations, and visualization.

As of the spring of 2010, development on Lumen has started to accelerate.
Here are the current documents available:

* How to [get and install][install] the current Lumen code.

Some informal documents related to the current code:

* An informal narrative description of [the Lumen library][nlumen].

* An informal narrative description of [the demo applications][ndemos].

And some older documents which still have some interesting information in them:

* The [original introduction page][intro].  Somewhat outdated, but still useful.

* The [original rationale][rat], also somewhat outdated, but still very much
  relevant.

Look for more to be added to this page in the days and weeks ahead!

<h2 class="centered">Bug Reports</h2>

You may report bugs in Lumen, or ask about difficulties in using it,
on the \#Ada IRC channel on [the Freenode IRC network][freenode].  And
though we haven't tested it yet, presumably you can also use
[the Github issues page][issues] for the Lumen project.

<h2 class="centered">News About Lumen</h2>

I'm going to start adding notable events here, so you can check quickly
whether there's something new since you last visited the site.

* **Thursday, 12 May 2011**: Several changes in the past few days:
    - Some additions to Lumen.GL and Lumen.GLU contributed by Julian.
    - New directory structure, including `bin` and `data` dirs, and
      a `demo/obj` subdir to keep things tidy.
    - Added NeHe "lesson" demos, contributed by Julian.
    - Added thin OpenGL bindings contributed by Rod Kay.
    - Converted all demos to use Lumen.GL and Lumen.GLU instead of the
      raw thin bindings.
    - Replicated git repository to [github][].

* **Wednesday, 22 Dec 2010**: New visual-choosing code, should allow operation
    on more systems.  Also integrated Julian's latest BMP image loading
    changes, supporting a wider variety of BMP formats.

* **Monday, 19 Jul 2010**: Added support for joysticks; probably
    Linux-specific for now.

* **Sunday, 18 Jul 2010**: Added loading of BMP images, courtesy of Julian
    Leyh.

* **Tuesday, 13 Jul 2010**: Raw keycodes can now be translated to Latin-1
    characters, and extended character codes for non-Latin-1 keystrokes.

* **Thursday, 1 Jul 2010**: Added the Lumen.Image package and the `texture`
    demo to show it in action.

* **Sunday, 20 Jun 2010**: Library is now portable between 64-bit and 32-bit
    systems.

* **Saturday, 29 May 2010**: Added animation support, and a third demo app to
    show it off.

* **Saturday, 15 May 2010**: Lumen project begins adding code and
    documentation.  Yay!


[freenode]:  http://freenode.net/
[github]:    https://github.com/karakalo/lumen
[install]:   install.html
[intro]:     old-intro.html
[issues]:    https://github.com/karakalo/lumen/issues
[ndemos]:    narrative-demos.html
[nlumen]:    narrative-lumen.html
[rat]:       rationale.html
