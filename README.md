# Shakebook - Robust Technical Documentation

Shakebook is a documentation generator aimed at covering all the bases for
mathematical, technical and scientific diagrams and typesetting. Currently
powered by [shake](https://shakebuild.com/), [pandoc](https://pandoc.org/),
[slick](https://hackage.haskell.org/package/slick),
[inline-r](https://tweag.github.io/HaskellR/),
[diagrams](https://archives.haskell.org/projects.haskell.org/diagrams/) and
[dihaa](https://bitbucket.org/sascha_wilde/dihaa), designed for gitlab-ci.
Shakebook is "open-hood", meaning there's no api surface. Everything you need
is in the Shakefile and the supporting periphery. Using shake, you can isolate
the diagrams and supporting compilation units that make up your document more
precisely than is available in typical LaTeX setups.

## Building Locally

You can build with either nix or stack. Nix is preferred, 

### Building With Nix

Drop into a reproducible build environment with

    nix-shell

If you have [cachix](https://cachix.org/), you can speed this up by running

    cachix use locallycompact

You can then run `shake` from within the nix shell.

    shake

You can test the ouput in chromium by running

    shake test
D
You can build the pdf and slide copies with

    shake pdf
    shake beamer

Clean up with

    shake clean

### Building With Stack

You will need stack.

    stack build

You will also need to install dihaa as it is the only dependency not picked up
as a library.

    stack install dihaa

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
