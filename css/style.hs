#!/usr/bin/env stack
-- stack runhaskell --package rio --package clay

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Clay as C
import RIO hiding (display, div, (&))

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
    padding (em 1) (em 0) (em 1) (em 0)
    margin (px 0) (px 0) (px 0) (px 0)
    textAlign center
    fontSize (pct 250)
  h2 ? do
    fontSize (pct 175)
    padding (em 0.5) (em 0) (em 0.5) (em 0)
    margin (px 0) (px 0) (px 0) (px 0)
  h3 ? do
    fontSize (pct 150)
  h4 ? do
    fontSize (pct 125)
  pre ? do
    padding (px 10) (px 20) (px 10) (px 20)
    margin (px 0) (px 0) (px 0) (px 0)
    code ? do
      fontFamily ["Source Code Pro"] [monospace]
      fontSize (px 13)
  code ? do
    color inherit
    backgroundColor inherit
  img ? do
    maxWidth (pct 100)
    height auto
  hr ? do
    borderBottom solid (px 1) "#EEEEEE"
  ".document-title" ? do
    fontSize (pct 200)
    textAlign center
    paddingBottom (px 0)
  ".document-version" ? do
    fontSize (pct 150)
    color    "#666666"
    textAlign center
    marginTop (px 0)
  ".author" ? do
    color    "#666666"
    fontWeight bold
    textAlign center 
  img ? do
    margin (px 0) auto (px 0) auto
  ul # ".sections" ? do
    padding (px 0) (px 0) (px 5) (px 0)
    margin (px 0) (px 0) (px 0) (px 0)
    li <? div <? do
      boxSizing borderBox
