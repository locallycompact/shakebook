# Hello World

Shakebook is a documentation generator aimed at covering all the bases for
mathematical, technical and scientific diagrams and typesetting. Currently
powered by [shake](https://shakebuild.com/), [pandoc](https://pandoc.org/),
[slick](https://hackage.haskell.org/package/slick),
[inline-r](https://tweag.github.io/HaskellR/),
[diagrams](https://archives.haskell.org/projects.haskell.org/diagrams/) and
[dihaa](https://bitbucket.org/sascha_wilde/dihaa), designed for gitlab-ci.
Shakebook has no API or library, everything you need is in the Shakefile and
the supporting periphery. Using shake, you can isolate the diagrams and
supporting compilation units that make up your document more precisely than is
available in typical LaTeX setups.

This example is powered by [this template
repository](https://gitlab.com/zenhaskell/shakebook) and deployed with gitlab
pages. To use it simply fork the repository on gitlab and the ci will take care
of the rest.

You can use the same markdown to generate an [html copy](index.html), [pdf
copy](book.pdf), or [slides](slides.pdf).

# Features

## Inline LaTeX

$x^2 + y^2 = e^{i\theta}$

## Syntax Highlighting

```{.haskell}
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

## Diagrams

Diagram generation with diagrams:

![](diagrams/tournament.svg){width=200 height=200}

## Plots

Plot generation with inline-r:

![](plots/cluster.png){width=200 height=200}

## Drawings

Drawings with dihaa

![](drawings/system.asc.png){width=400 height=400}

# Building Locally

You can build with either nix or stack, nix is preferred but both will work.

## Building With Nix

Drop into a reproducible build environment with

    nix-shell

If you have [cachix](https://cachix.org/), you can speed this up by running

    cachix use locallycompact

You can then run `shake` from in within the nix shell.

    shake

A shortcut to testing the output in chromium is

    shake test

You can build the pdf and slide copies with

    shake pdf
    shake beamer

Clean up with

    shake clean

## Building With Stack

Compile the `site` executable with

    stack build

You can then test this in chromium by doing

    stack exec -- site test

You can build the pdf and slide copies with

    stack exec -- site pdf
    stack exec -- site beamer

or change the browser by editing the Shakefile.hs.

# Roadmap

* Add [TikZ](http://www.texample.net/tikz/) support (and pandoc filters as compilation units in general)
* Add [clay](http://hackage.haskell.org/package/clay) and [lucid](http://hackage.haskell.org/package/lucid) support.
* EDSL for constructing [string
  diagrams](https://arxiv.org/pdf/1401.7220.pdf) and [signal flow
diagrams](https://arxiv.org/pdf/1803.05316.pdf) from monoidal categories.
* Support for model checking and verification as a preprocessing step.
* Calendar EDSL with a good time/event/duration abstraction.
* Better solution to Javascript.
* shakebook-init that deploys a template with features turned on/off.
