{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import Control.Lens
import Data.Default
import Data.Yaml
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import RIO hiding (view)
import qualified RIO.ByteString.Lazy as LBS
import RIO.Text as Text
import RIO.Set as Set
import Slick
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Text.Pandoc.PDF
import Text.Pandoc.Options
import Text.Pandoc.Templates
import Text.Pandoc.Readers
import Text.Pandoc.Writers

--- Global Configuration -----------------------------------------------------------

site :: FilePath
site = "public"

browser :: FilePath
browser = "chromium"

meta :: [FilePattern]
meta  = ["meta.txt"]

css :: [FilePattern]
css = ["css/*.css"]

fonts :: [FilePattern]
fonts = ["fonts/*.ttf"]

imgs :: [FilePattern]
imgs  = ["img/*.png"]

js :: [FilePattern]
js  = ["js/*.js"]

mdwn :: [FilePattern]
mdwn  = ["notes//*.md"]

htemplate :: FilePath
htemplate = "resources/page.tmpl"

margin :: String
margin = "3cm"

--- Verbatim Files ------------------------------------------------------------

verbatim :: Iso' FilePath FilePath
verbatim = iso (site </>) dropDirectory1

copyVerbatim :: FilePath -> Action ()
copyVerbatim = flip copyFile' <*> (view . from $ verbatim)

getCSSFiles      = getDirectoryFiles "" css
getFonts         = getDirectoryFiles "" fonts
getImages        = getDirectoryFiles "" imgs
getJSFiles       = getDirectoryFiles "" js

--- Pandoc Options -------------------------------------------------------------

markdownReaderOptions :: ReaderOptions
markdownReaderOptions = def {readerExtensions = pandocExtensions }

latexWriterOptions :: WriterOptions
latexWriterOptions = def { writerTableOfContents = True }

beamerWriterOptions :: WriterOptions
beamerWriterOptions = def { writerVariables = [("fonttheme", "serif")] }

html5WriterOptions :: WriterOptions
html5WriterOptions = def { writerTableOfContents = True }

--- PDF Compilation ------------------------------------------------------------

compileMarkdown mdwn act out = do
  x <- getDirectoryFiles "" mdwn
  y <- mapM readFile' x
  f <- act . Text.pack . join $ y
  LBS.writeFile out f

makePDFWith mdwn writer writerOptions template = compileMarkdown mdwn $ \x -> do
  f <- liftIO . runIOorExplode $ do
    p <- readMarkdown markdownReaderOptions x
    k <- template
    makePDF "pdflatex" [] writer writerOptions { writerTemplate = Just k } p
  either (fail . show) return f

makePDFBeamer :: [FilePattern] -> FilePath -> Action ()
makePDFBeamer mdwn = makePDFWith mdwn writeBeamer beamerWriterOptions (getDefaultTemplate "beamer")

makePDFLaTeX :: [FilePattern] -> FilePath -> Action ()
makePDFLaTeX mdwn = makePDFWith mdwn writeLaTeX latexWriterOptions (getDefaultTemplate "latex")

--- HTML Compilation --------------------------------------------------------

makeHTML5With mdwn writer writerOptions template = compileMarkdown mdwn $ \x -> do
  k <- template
  (v :: Page) <- loadUsing' (readMarkdown markdownReaderOptions)
                             (writer writerOptions { writerTemplate = Just k }) x
  return $ LBS.fromStrict $ encodeUtf8 (content v)

data Page = Page {
  content :: Text
} deriving (Eq, Show, Generic, FromJSON, ToJSON)

--- Just Shake To Build ------------------------------------------------------

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
    need $ view verbatim <$> join [css, fonts, imgs, js]
    need [htemplate]
    makeHTML5With (mdwn <> meta) writeHtml5String html5WriterOptions { writerVariables = ("css",) <$> css } (readFile' htemplate) out

  pdf %> makePDFLaTeX (mdwn <> meta)

  beamer %> makePDFBeamer (mdwn <> meta)

  phony "pdf"    $ need [pdf]
  phony "beamer" $ need [beamer]
  phony "test"   $ need [index] >> cmd browser index
