# Changelog for shakebook

## v0.0.3.1

* Switch to MathJax for rendering.

## v0.0.3.0

* Change cachix CI to use `CACHIX_SIGNING_KEY` (and actually work).
* Drop locale export in CI it's now correctly exported in the upstream docker.

## v0.0.2.0

* Add nix-shell with stack2nix environment.
* Add cachix support in .gitlab-ci.yml

## v0.0.1.0

* Initial draft of shakebook.
* Allows for extensible pandoc-based markdown/latex book development.
* Comes pre-equipped with support for [inline-r](https://tweag.github.io/HaskellR/),
  [diagrams](https://archives.haskell.org/projects.haskell.org/diagrams/) and
  [dihaa](http://hackage.haskell.org/package/dihaa) drawings.
