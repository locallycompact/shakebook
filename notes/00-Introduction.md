# Hello World

Shakebook is a robust documentation generator powered by
[shake](https://shakebuild.com/), [pandoc](https://pandoc.org/)
[slick](https://hackage.haskell.org/package/slick),
[inline-r](https://tweag.github.io/HaskellR/),
[diagrams](https://archives.haskell.org/projects.haskell.org/diagrams/) and
[dihaa](https://bitbucket.org/sascha_wilde/dihaa), designed for gitlab-ci.
Shakebook is "open-hood", meaning there's no api surface. Everything you need
is in the Shakefile and the supporting periphery.

This example is powered by [this template
repository](https://gitlab.com/zenhaskell/shakebook) and deployed with gitlab
pages. To use it simply fork the repository on gitlab and the ci will take care
of the rest.

You can use the same markdown to generate an [html copy](index.html), [pdf
copy](book.pdf), or [slides](slides.pdf).

# Features

## Inline LaTeX:

$x^2 + y^2 = e^{i\theta}$

## Diagrams

Diagram generation with diagrams:

![](diagrams/tournament.svg){width=200 height=200}

## Plots

Plot generation with inline-r:

![](plots/cluster.png){width=200 height=200}

## Drawings

Drawings with dihaa

![](drawings/system.asc.png){width=400 height=400}

# Testing Locally

You will need stack.

    stack build

You can then test this in chromium by doing

    stack exec -- site test

You can build the pdf and slide copies with

    stack exec -- site pdf
    stack exec -- site beamer

or change the browser by editing the Shakefile.hs.
