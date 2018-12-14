# Shakebook - Robust Technical Documentation

Shakebook is a robust documentation generator powered by
[shake](https://shakebuild.com/), [pandoc](https://pandoc.org/),
[slick](https://hackage.haskell.org/package/slick),
[inline-r](https://tweag.github.io/HaskellR/),
[diagrams](https://archives.haskell.org/projects.haskell.org/diagrams/) and
[dihaa](https://bitbucket.org/sascha_wilde/dihaa), designed for gitlab-ci.
Shakebook is "open-hood", meaning there's no api surface. Everything you need
is in the Shakefile and the supporting periphery.

## Building Locally

You can build with either stack or nix.

### Building With Nix

Drop into a reproducible build environment with

    nix-shell

You can then run `shake` from in here. You can test this in chromium by running.

    shake

You can build the pdf and slide copies with

    shake pdf
    shake beamer

Clean up with

    shake clean

### Building With Stack

You will need stack.

    stack build

This will create the site in the folder `public`. This will take a while the
first time you run it.

You can then test this in chromium by doing

    stack exec -- site test

You can build the pdf and slide copies with

    stack exec -- site pdf
    stack exec -- site beamer

or change the browser by editing the Shakefile.hs.

You can clean up with

    stack exec -- site clean

If anything goes haywire, just `git clean -dfx` and rebuild from scratch.
