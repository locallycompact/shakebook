#!/usr/bin/env stack
-- stack runhaskell --package rio --package clay

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Clay as C
import RIO hiding (display, (&))

main = putCss $ do
  html <> body ? do
    margin (px 0) (px 0) (px 0) (px 0)
    padding (px 0) (px 0) (px 0) (px 0)
    height (pct 100)
    fontFamily ["Source Sans Pro", "Helvetica Neue", "Helvetica"] [sansSerif]
    fontSize (px 15)
    fontStyle normal
    fontWeight (weight 400)
    lineHeight (px 1.5)
    color "#333333"
  h1 <> h2 <> h3 <> h4 <> h5 ? do
    fontWeight bold
    lineHeight (px 1.1)
    margin (em 1) (em 0) (em 0.5) (em 0)
  h1 ? do
    padding (em 1) (em 0) (em 0) (em 0)
    margin (px 0) (px 0) (px 0) (px 0)
    textAlign center
    fontSize (pct 250)
  h2 ? do
    fontSize (pct 175)
  h3 ? do
    fontSize (pct 150)
  h4 ? do
    fontSize (pct 125)
