# gitbook-template

This is a simple [shake](https://shakebuild.com) project for making gitbooks in
markdown and [pandoc](https://pandoc.org/). It automatically compiles in
gitlab's CI to produce an HTML book.

How to use:

    stack install pandoc shake
    stack exec shake test
