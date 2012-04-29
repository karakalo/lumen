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

* **Thursday, 5 April 2012**: Work resumes on Lumen with the addition of a
    couple of [simple old apps][stages] I had lying around, that I recently
    ported to use Lumen rather than GtkAda.  They're not much, but may be
    useful to somebody sometime.

* **Wednesday, 14 December 2011**: Added a [ramble about widgets][ramble]
    intended to spark discussion about the design of Lumen widgets.  It was
    aimed at anyone who cares how widgets are done in Lumen, so if you have
    ideas about that, please do let us know.  The #Ada IRC channel on Freenode
    is the place such discussions happen, but we could use email if you
    prefer.

* **Tuesday, 13 December 2011**: David updated the streamlined bindings
    (`Lumen.GL`) to adhere to the [Ada 95 Quality and Style Guide][aqsg], a
    change that seemed appropriate.

* **Tuesday, 6 December 2011**: The documentation on this site was updated, to
    report the latest news and to include narratives for Luke's
    [blending demo][nblend] and David's [fire demo][nfire].

* **Monday, 5 December 2011**: David joins the project in a big way, by
    contributing the "fire" demo!  It's a very pretty demo, depicting a nice
    "flame" effect using particles and blending.  His OpenGL expertise will be
    most welcome.

* **Sunday, 4 September 2011**: Luke added a new blending demo, and the calls
    necessary to support it to the streamlined bindings (Lumen.GL).  And some
    new image files for it to use.  It's very pretty; do give it a look.  Docs
    for it are forthcoming.

* **Tuesday, 30 August 2011**: Felix made a bunch of changes:
    - Restructured the project source a bit, moving Lumen.GL to the regular
      Lumen source directory.
    - Added preliminary wgl (MS-Windows) and cgl (Mac OS-X) binding support.

    He has expressed an interest in continuing to modify the thin bindings, so
    we'll keep an eye out for his future changes!

* **Thursday, 12 May 2011**: Several changes in the past few days:
    - Some additions to Lumen.GL and Lumen.GLU contributed by Julian.
    - New directory structure, including `bin` and `data` dirs, and
      a `demo/obj` subdir to keep things tidy.
    - Added NeHe "lesson" demos, contributed by Julian.
    - Added thin OpenGL bindings contributed by Rod Kay.
    - Converted all demos to use Lumen.GL and Lumen.GLU instead of the
      raw thin bindings.
    - Replicated git repository to [github][].

* Older news items are now on a [separate page][oldnews].

[aqsg]:      http://www.adaic.com/docs/95style/html/cover.html
[freenode]:  http://freenode.net/
[github]:    https://github.com/karakalo/lumen
[install]:   install.html
[intro]:     old-intro.html
[issues]:    https://github.com/karakalo/lumen/issues
[nblend]:    narrative-blending.html
[ndemos]:    narrative-demos.html
[nfire]:     narrative-fire.html
[nlumen]:    narrative-lumen.html
[rat]:       rationale.html
[ramble]:    widgets-1.html
[oldnews]:   old-news.html
[stages]:    narrative-stages.html
