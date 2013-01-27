Title: Older Lumen Project News

<h1 class="centered">Older Lumen Project News</h1>

This page contains news items that got old and were crowded off the front page.

## 2011

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

## 2010

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

[aqsg]:      http://www.adaic.com/docs/95style/html/cover.html
[github]:    https://github.com/karakalo/lumen
[nblend]:    narrative-blending.html
[nfire]:     narrative-fire.html
