#!/usr/bin/env stack
-- stack runhaskell --package rio --package clay --resolver lts-12.8

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Clay as C
import RIO hiding (display, div, (&))

main = putCss $ do
  main_ ? do
    position fixed
    top (px 0)
    right (px 0)
    left (px 250)
    bottom (px 0)
    height (pct 100)
    overflowY auto
    "-webkit-overflow-scrolling" -: "touch"
  article ? do
    padding (px 20) (px 20) (px 20) (px 20)
    maxWidth (px 800)
  nav ? do
    position    fixed
    top         (px 0)
    left        (px 0)
    right       (px 0)
    height      (pct 100)
    width       (px 250)
    overflowX   auto
    overflowY   hidden
    background  ("#F7F7F7" :: Color)
    borderRight solid (px 1) "#EEEEEE"
    "-webkit-overflow-scrolling" -: "touch"
    ".logo" ? do
      display   block
      margin    (px 10) 0 0 0
      textAlign center
    img ? do
      width (px 160)
    "#toggle" ? do
      display none
  ".toc" ? do
    margin (px 0) (px 0) (px 0) (px 0)
    padding (px 0) (px 0) (px 0) (px 0)
    fontSize (px 13)
    li ? a ? do
      display block
      textDecoration none
      color inherit
      ":hover" & textDecoration underline
    ul ? do
      listStyleType circleListStyle
      padding (px 10) (px 10) (px 10) (px 20)
    ul <? do
      listStyleType none
      padding (px 0) (px 0) (px 10) (px 0)
      li <? a <? fontWeight bold
