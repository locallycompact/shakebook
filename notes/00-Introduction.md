# Hello World

Shakedoc is a robust documentation generator powered by shake, pandoc, latex, slick, inline-r, diagrams and dihaa, designed for
gitlab-ci.

This example is powered by [this template repository](https://gitlab.com/zenhaskell/shake) and deployed with gitlab pages. To use it simply fork the repository on gitlab and the ci will take care of the rest.

You can use the same markdown to generate an [html copy](index.html), [pdf copy](book.pdf), or [slides](slides.pdf).

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

![](diagrams/drawing.png){width=400 height=400}

# Testing Locally

You will need haskell and stack, you will also need to install pandoc and shake with:

    stack install pandoc shake

You can then test this in chromium by doing

    stack exec shake test

or change the browser by editing the Shakefile.hs.
