#!/usr/bin/env stack
-- stack runhaskell --resolver lts-12.12 --package rio --package inline-r --package optparse-applicative

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

import H.Prelude as H
import Language.R.QQ
import RIO
import qualified RIO.ByteString as BS
import Options.Applicative
import System.Random

out :: Parser String
out = strOption
    ( long "output"
   <> short 'o'
   <> metavar "FILENAME" )

opts :: ParserInfo String
opts = info (out <**> helper)
  ( fullDesc
  <> progDesc "Dumps this R script to a png."
  <> header "R single-shot png printer." )

main = do
  path <- execParser opts
  H.withEmbeddedR defaultConfig $ do
    H.runRegion $ do
    -- Put any complex model here
      std <- io $ newStdGen
      let (xs::[Double]) = take 100 $ randoms std
      [r| png(path_hs, width=480, height=480); |]
      d  <- [r| matrix(xs_hs,ncol = 2) |]
      rv <- [r| clusters <- kmeans(d_hs, 2) |]
      [r| par(mar = c(5.1, 4.1, 0, 1));
        plot(d_hs, col = rv_hs$cluster, pch = 20
            , cex = 3, xlab = "x", ylab = "y");
        points(rv_hs$centers, pch = 4, cex = 4, lwd = 4);
      |]
      return ()
