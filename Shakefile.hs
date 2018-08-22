{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

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

mdwn :: [FilePattern]
mdwn = ["notes//*.md"]

htemplate :: FilePath
htemplate = "resources/page.tmpl"

margin :: String
margin = "3cm"

--- Verbatim Files ------------------------------------------------------------

fonts :: [FilePattern]
fonts = ["fonts/*.ttf"]

imgs :: [FilePattern]
imgs = ["img/*.png"]

js :: [FilePattern]
js = ["js/*.js"]

verbatim :: Iso' FilePath FilePath
verbatim = iso (site </>) dropDirectory1

copyVerbatim :: FilePath -> Action ()
copyVerbatim = flip copyFile' <*> (view . from $ verbatim)

getFonts :: Action [FilePath]
getFonts = getDirectoryFiles "" fonts

getImages :: Action [FilePath]
getImages = getDirectoryFiles "" imgs

getJSFiles :: Action [FilePath]
getJSFiles = getDirectoryFiles "" js

verbatimFileRules :: Rules ()
verbatimFileRules = forM_ (view verbatim <$> join [fonts, imgs, js]) (%> copyVerbatim)

--- CSS ------------------------------------------------------------------------

css :: [FilePattern]
css = ["css/*.hs"]

getCSSFiles :: Action [FilePath]
getCSSFiles = getDirectoryFiles "" css

hsToCss :: Iso' FilePath FilePath
hsToCss = iso ((site </>) . (-<.> ".css")) (dropDirectory1 . (-<.> ".hs"))

compileCss :: FilePath -> Action ()
compileCss x = do
  Stdout z <- command [] ((view . from) hsToCss x) []
  writeFile' x z

cssRules :: Rules ()
cssRules = forM_ (view hsToCss <$> css) (%> compileCss)

--- R Plots --------------------------------------------------------------------

plots :: [FilePattern]
plots = ["plots/*.hs"]

plotToPng :: Iso' FilePath FilePath
plotToPng = iso ((site </>) . (-<.> ".png")) (dropDirectory1 . (-<.> ".hs"))

getPlots :: Action [FilePath]
getPlots = getDirectoryFiles "" plots

compilePlot :: FilePath -> Action ()
compilePlot x = command [] ((view . from) plotToPng x) ["-o", x] 

plotRules :: Rules ()
plotRules = forM_ (view plotToPng <$> plots) (%> compilePlot)

--- Diagrams -------------------------------------------------------------------

diagrams :: [FilePattern]
diagrams = ["diagrams/*.hs"]

diagramToSvg :: Iso' FilePath FilePath
diagramToSvg = iso ((site </>) . (-<.> ".svg")) (dropDirectory1 . (-<.> ".hs"))

getDiagrams :: Action [FilePath]
getDiagrams = getDirectoryFiles "" diagrams

compileDiagram :: FilePath -> Action ()
compileDiagram x = command [] ((view . from) diagramToSvg x) ["-o", x, "-w", "400", "-h", "400"]

diagramRules :: Rules ()
diagramRules = forM_ (view diagramToSvg <$> diagrams) (%> compileDiagram)
  
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

  verbatimFileRules
  cssRules
  plotRules
  diagramRules

  index %> \out -> do
    css   <- getCSSFiles
    fonts <- getFonts
    imgs  <- getImages
    js    <- getJSFiles
    plots <- getPlots
    diagrams <- getDiagrams
    need $ view verbatim <$> join [fonts, imgs, js]
    need $ view hsToCss <$> css
    need $ view plotToPng <$> plots
    need $ view diagramToSvg <$> diagrams
    x <- getDirectoryFiles "" $ mdwn <> meta
    y <- mapM readFile' x
    t <- readFile' htemplate
    putNormal $ show css
    f <- makeHTML5Page (Text.pack . join $ y)
                       html5WriterOptions { writerVariables = ("css", ) <$> (dropDirectory1 . view hsToCss <$> css) , writerTemplate = Just t }
    LBS.writeFile out f

  pdf %> \out -> do
    plots <- getPlots
    diagrams <- getDiagrams
    need $ view plotToPng <$> plots
    need $ view diagramToSvg <$> diagrams
    x <- getDirectoryFiles "" $ mdwn <> meta
    y <- mapM readFile' x
    f <- liftIO . runIOorExplode $ do
      p <- readMarkdown markdownReaderOptions $ Text.pack . join $ y
      makePDFLaTeX p
    LBS.writeFile out f

  beamer %> \out -> do
    plots <- getPlots
    diagrams <- getDiagrams
    need $ view plotToPng <$> plots
    need $ view diagramToSvg <$> diagrams
    x <- getDirectoryFiles "" $ mdwn <> meta
    y <- mapM readFile' x 
    f <- liftIO . runIOorExplode $ do
      p <- readMarkdown markdownReaderOptions $ Text.pack . join $ y
      makePDFBeamer p
    LBS.writeFile out f

  phony "pdf" $ need [pdf]
  phony "beamer" $ need [beamer]
  phony "test" $ need [index] >> cmd browser index
