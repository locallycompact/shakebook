{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import           Control.Lens
import           Data.Default
import           Data.Yaml
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import           RIO                     hiding ( view )
import qualified RIO.ByteString.Lazy           as LBS
import           RIO.Text                      as Text
import           RIO.Set                       as Set
import           Slick
import           Text.Pandoc.Class
import           Text.Pandoc.Definition
import           Text.Pandoc.PDF
import           Text.Pandoc.Options
import           Text.Pandoc.Templates
import           Text.Pandoc.Readers
import           Text.Pandoc.Writers

--- Global Configuration -----------------------------------------------------------

site :: FilePath
site = "public"

browser :: FilePath
browser = "chromium"

meta :: [FilePattern]
meta = ["meta.txt"]

css :: [FilePattern]
css = ["css/*.css"]

fonts :: [FilePattern]
fonts = ["fonts/*.ttf"]

imgs :: [FilePattern]
imgs = ["img/*.png"]

js :: [FilePattern]
js = ["js/*.js"]

mdwn :: [FilePattern]
mdwn = ["notes//*.md"]

htemplate :: FilePath
htemplate = "resources/page.tmpl"

margin :: String
margin = "3cm"

--- Verbatim Files ------------------------------------------------------------

verbatim :: Iso' FilePath FilePath
verbatim = iso (site </>) dropDirectory1

copyVerbatim :: FilePath -> Action ()
copyVerbatim = flip copyFile' <*> (view . from $ verbatim)

getCSSFiles = getDirectoryFiles "" css
getFonts = getDirectoryFiles "" fonts
getImages = getDirectoryFiles "" imgs
getJSFiles = getDirectoryFiles "" js

--- Pandoc Options -------------------------------------------------------------

markdownReaderOptions :: ReaderOptions
markdownReaderOptions = def { readerExtensions = pandocExtensions }

latexWriterOptions :: WriterOptions
latexWriterOptions = def { writerTableOfContents = True }

beamerWriterOptions :: WriterOptions
beamerWriterOptions = def { writerVariables = [("fonttheme", "serif")] }

html5WriterOptions :: WriterOptions
html5WriterOptions = def { writerTableOfContents = True }

--- PDF Compilation ------------------------------------------------------------

makePDFThrow :: String -> [String] -> (WriterOptions -> Pandoc -> PandocIO Text) -> WriterOptions -> Pandoc -> PandocIO LBS.ByteString
makePDFThrow a b c d e = makePDF a b c d e >>= either (fail . show) return

makePDFBeamer :: Pandoc -> PandocIO LBS.ByteString
makePDFBeamer p = do
  t <- getDefaultTemplate "beamer"
  makePDFThrow "pdflatex" [] writeBeamer beamerWriterOptions { writerTemplate = Just t } p

makePDFLaTeX :: Pandoc -> PandocIO LBS.ByteString
makePDFLaTeX p = do
  t <- getDefaultTemplate "latex"
  makePDFThrow "pdflatex" [] writeLaTeX latexWriterOptions { writerTemplate = Just t } p

--- HTML Compilation --------------------------------------------------------

makeHTML5Page mdwn writerOptions = do
  (v :: Page) <- loadUsing' (readMarkdown markdownReaderOptions) (writeHtml5String writerOptions) mdwn
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
    x <- getDirectoryFiles "" $ mdwn <> meta
    y <- mapM readFile' x
    t <- readFile' htemplate
    f <- makeHTML5Page (Text.pack . join $ y)
                       html5WriterOptions { writerVariables = ("css", ) <$> css , writerTemplate = Just t }
    LBS.writeFile out f

  pdf %> \out -> do
    x <- getDirectoryFiles "" $ mdwn <> meta
    y <- mapM readFile' x
    f <- liftIO . runIOorExplode $ do
      p <- readMarkdown markdownReaderOptions $ Text.pack . join $ y
      makePDFLaTeX p
    LBS.writeFile out f

  beamer %> \out -> do
    x <- getDirectoryFiles "" $ mdwn <> meta
    y <- mapM readFile' x 
    f <- liftIO . runIOorExplode $ do
      p <- readMarkdown markdownReaderOptions $ Text.pack . join $ y
      makePDFBeamer p
    LBS.writeFile out f

  phony "pdf" $ need [pdf]
  phony "beamer" $ need [beamer]
  phony "test" $ need [index] >> cmd browser index
