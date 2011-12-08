Title: Narrative Description of "fire Demo

This is a simple narrative description of the `fire` application included with
the Lumen library as a demo.

The `fire` demo requires no command-line parameters, so you just invoke it by
typing its name, like `./fire` if you're sitting in the `bin` directory, or
`bin/fire` if you're in the `lumen` directory.

When run, `fire` creates a black window with a "fire" at the bottom, created
using a particle system and blending of carefully selected colors.  Terminate
it by pressing any key, or closing the window.

The `fire` demo uses a custom event loop instead of any of the canned mainloop
procedures.  It runs as fast as possible, "flat out" in Lumen terms, reporting
the effective frame rate in the window's title bar.

Here is the author's description of the process the app uses to create its image:

> It's point sprites [textured quads in screen space] (lots of them) and
> additive blending and a tricky color choice.
>
> Here's how it works:
>
> All particles have a life time of 2 s. And they are initialized with
> different ages, so as to uniformly "fill" 2 seconds. This means: When
> updating the age per frame, some particles "die" and are respawned, which
> means that the spawning frequency and the number of particles never change.
>
> They respawn at (0.0, -0.5) and their velocity is initialized to a uniform
> random square [-spread, spread] x [-spread, spread].  And the respawn is the
> same as the initialization.
>
> Global lift (or inverse gravity) is applied to the velocity per frame. This
> causes the particles to rise.
>
> That's all there is to it regarding the dynamics.
