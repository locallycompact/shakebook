# gitbook-template

This is a simple [shake](https://shakebuild.com) project for making gitbooks in
markdown and [pandoc](https://pandoc.org/). It automatically compiles in gitlab
CI to produce [HTML](https://zenhaskell.gitlab.io/gitbook-template) and
[pdf](https://zenhaskell.gitlab.io/gitbook-template/book.pdf) copies.

How to use:

    stack install pandoc shake
    stack exec shake test ## Edit the 'browser' value in `Shakefile.hs`
    stack exec shake pdf  ## Makes a pdf copy
