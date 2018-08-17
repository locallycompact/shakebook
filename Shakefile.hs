{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Lens
import Data.Default
import Data.Yaml
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import RIO hiding (view)
import RIO.ByteString.Lazy as LBS
import RIO.Text as Text
import RIO.Set as Set
import Slick
import Text.Pandoc.Class
import Text.Pandoc.PDF
import Text.Pandoc.Options
import Text.Pandoc.Templates
import Text.Pandoc.Readers
import Text.Pandoc.Writers

site :: FilePath
site = "public"

browser :: FilePath
browser = "chromium"

meta :: [FilePath]
meta  = ["meta.txt"]

css :: [FilePath]
css = ["css/*.css"]

fonts :: [FilePath]
fonts = ["fonts/*.ttf"]

imgs :: [FilePath]
imgs  = ["img/*.png"]

js :: [FilePath]
js  = ["js/*.js"]

mdwn :: [FilePath]
mdwn  = ["notes//*.md"]

htemplate :: FilePath
htemplate = "resources/page.tmpl"

tocOpts :: [String]
tocOpts = ["--toc", "--toc-depth=2"]

margin :: String
margin = "3cm"

getCSSFiles      = getDirectoryFiles "" css
getFonts         = getDirectoryFiles "" fonts
getImages        = getDirectoryFiles "" imgs
getJSFiles       = getDirectoryFiles "" js

verbatim :: Iso' FilePath FilePath
verbatim = iso (site </>) dropDirectory1

copyVerbatim :: FilePath -> Action ()
copyVerbatim = flip copyFile' <*> (view . from $ verbatim)

pandocCmd :: [FilePath] -> [String] -> FilePath -> Action ()
pandocCmd files opts out = command [] "pandoc" $ files <> ["-o", out, "-s"] <> opts

compileMarkdown :: [FilePath] -> [String] -> FilePath -> Action ()
compileMarkdown mdwn opts out = getDirectoryFiles "" mdwn >>= \x -> pandocCmd x opts out

data Page = Page {
  content :: Text
} deriving (Eq, Show, Generic, FromJSON, ToJSON)

main :: IO ()
main = shakeArgs shakeOptions $ do
  let index  = site </> "index.html"
  let pdf    = site </> "book.pdf"
  let beamer = site </> "slides.pdf"

  want [index]

  phony "clean" $ do
    putNormal $ "Cleaning files in " ++ site
    removeFilesAfter "." [site]

  let supports  = join [css, fonts, imgs, js]
  let supportsT = view verbatim <$> supports

  forM_ supportsT $ flip (%>) copyVerbatim

  index %> \out -> do
    css   <- getCSSFiles
    fonts <- getFonts
    imgs  <- getImages
    js    <- getJSFiles
    let cssOpts = css >>= (\x -> ["-c", x])
    need $ view verbatim <$> join [css, fonts, imgs, js]
    need [htemplate]
    let opts = ["--template", htemplate,
                "-t", "html", "-f", "markdown",
                "--highlight-style", "pygments",
                "--mathjax"] ++ cssOpts ++ tocOpts
    compileMarkdown mdwn opts out

  pdf %> \out -> do
    x <- getDirectoryFiles "" mdwn
    y <- mapM readFile' $ meta <> x
    f <- liftIO $ runIOorExplode $ do
      p <- readMarkdown def {readerExtensions = pandocExtensions} (Text.pack $ join y)
      d <- getDefaultTemplate "latex"
      makePDF "pdflatex" [] writeLaTeX def {writerTemplate=Just d, writerTableOfContents = True} p
    either (fail . show) return f >>=  LBS.writeFile out

  beamer %> compileMarkdown mdwn ["-t", "beamer"]

  phony "pdf"    $ need [pdf]
  phony "beamer" $ need [beamer]
  phony "test"   $ need [index] >> cmd browser index
