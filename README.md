# Shakebook - Robust Technical Documentation

Shakebook is a robust documentation generator powered by
[shake](https://shakebuild.com/), [pandoc](https://pandoc.org/)
[slick](https://hackage.haskell.org/package/slick),
[inline-r](https://tweag.github.io/HaskellR/),
[diagrams](https://archives.haskell.org/projects.haskell.org/diagrams/) and
[dihaa](https://bitbucket.org/sascha_wilde/dihaa), designed for gitlab-ci.
Shakebook is "open-hood", meaning there's no api surface. Everything you need
is in the Shakefile and the supporting periphery.

# Testing Locally

You will need stack.

    stack build

You can then test this in chromium by doing

    stack exec -- site test

You can build the pdf and slide copies with

    stack exec -- site pdf
    stack exec -- site beamer

or change the browser by editing the Shakefile.hs.
