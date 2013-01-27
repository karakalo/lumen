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

* **Wednesday, 23 January 2013**: The long-awaited and much-delayed Lumen
    repository re-organization is now complete!  See [this page][changelog]
    for a summary of the major changes.  Note that some of the more
    experimental code branches have not been integrated with the new
    repositories; until they are, to find that code look for a repository
    called `lumen-old`.

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

* Older news items are now on a [separate page][oldnews].

[changelog]: reorg-changes.html
[freenode]:  http://freenode.net/
[install]:   install.html
[intro]:     old-intro.html
[issues]:    https://github.com/karakalo/lumen/issues
[ndemos]:    narrative-demos.html
[nlumen]:    narrative-lumen.html
[oldnews]:   old-news.html
[ramble]:    widgets-1.html
[rat]:       rationale.html
[stages]:    narrative-stages.html
