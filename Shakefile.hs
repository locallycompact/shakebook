{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

import           Control.Lens            hiding ((<.>))
import           Data.Default
import           Data.Yaml
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import           RIO                     hiding ( view )
import qualified RIO.ByteString.Lazy           as LBS
import           RIO.Directory
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

css :: [FilePattern]
css = ["css/*.css"]

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

getCSSFiles :: Action [FilePath]
getCSSFiles = getDirectoryFiles "" css

staticFileDeploy :: [FilePattern]
staticFileDeploy = view (mapping verbatim) $ join [fonts, imgs, js, css]

staticFileDeployRules :: Rules ()
staticFileDeployRules = forM_ staticFileDeploy (%> copyVerbatim)

--- Extension Isomorphisms -----------------------------------------------------

hsToCss :: Iso' FilePath FilePath
hsToCss = iso (-<.> ".css") (-<.> ".hs")

hsToPng :: Iso' FilePath FilePath
hsToPng = iso (-<.> ".png") (-<.> ".hs")

hsToSvg :: Iso' FilePath FilePath
hsToSvg = iso (-<.> ".svg") (-<.> ".hs")

addPngExt :: Iso' FilePath FilePath
addPngExt = iso (<.> ".png") dropExtension

--- R Plots --------------------------------------------------------------------

plots :: [FilePattern]
plots = ["plots/*.hs"]

getPlots :: Action [FilePath]
getPlots = getDirectoryFiles "" plots

plotResult :: [FilePattern]
plotResult = view (mapping hsToPng) plots

plotDeploy :: [FilePattern]
plotDeploy = view (mapping verbatim) plotResult

compilePlot :: FilePath -> Action ()
compilePlot x = do
  let src = (view . from) hsToPng x
  need [src]
  command [] src ["-o", x] 

plotCompileRules :: Rules ()
plotCompileRules = forM_ plotResult (%> compilePlot)

plotDeployRules :: Rules ()
plotDeployRules = forM_ plotDeploy (%> copyVerbatim)

--- Diagrams -------------------------------------------------------------------

diagrams :: [FilePattern]
diagrams = ["diagrams/*.hs"]

getDiagrams :: Action [FilePath]
getDiagrams = getDirectoryFiles "" diagrams

diagramResult :: [FilePattern]
diagramResult = view (mapping hsToSvg) diagrams

diagramDeploy :: [FilePattern]
diagramDeploy = view (mapping verbatim) diagramResult

compileDiagram :: FilePath -> Action ()
compileDiagram x = do
  let src = (view . from) hsToSvg x
  need [src]
  command [] src ["-o", x, "-w", "200", "-h", "200"]

diagramCompileRules :: Rules ()
diagramCompileRules = forM_ diagramResult (%> compileDiagram)

diagramDeployRules :: Rules ()
diagramDeployRules = forM_ diagramDeploy (%> copyVerbatim)

--- Dihaa Drawings ------------------------------------------------------------

drawings :: [FilePattern]
drawings = ["drawings/*.asc"]

getDrawings :: Action [FilePath]
getDrawings = getDirectoryFiles "" drawings

drawingResult :: [FilePattern]
drawingResult = view (mapping addPngExt) drawings

drawingDeploy :: [FilePattern]
drawingDeploy = view (mapping verbatim) drawingResult

compileDrawing :: FilePath -> Action ()
compileDrawing x = do
  let src = (view . from) addPngExt x
  need [src]
  command [] "dihaa" [src, "-p"]

drawingCompileRules :: Rules ()
drawingCompileRules = forM_ drawingResult (%> compileDrawing)

drawingDeployRules :: Rules ()
drawingDeployRules = forM_ drawingDeploy (%> copyVerbatim)

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

  staticFileDeployRules
  plotCompileRules
  plotDeployRules
  diagramCompileRules
  diagramDeployRules
  drawingCompileRules
  drawingDeployRules

  index %> \out -> do
    css   <- getCSSFiles
    fonts <- getFonts
    imgs  <- getImages
    js    <- getJSFiles
    plots <- getPlots
    diagrams <- getDiagrams
    drawings <- getDrawings
    need $ view (mapping verbatim) $ join [fonts, imgs, js, css]
      <> view (mapping hsToPng) plots
      <> view (mapping hsToSvg) diagrams
      <> view (mapping addPngExt) drawings
    x <- getDirectoryFiles "" $ mdwn <> meta
    y <- mapM readFile' x
    t <- readFile' htemplate
    f <- makeHTML5Page (Text.pack . join $ y)
                       html5WriterOptions { writerVariables = ("css", ) . view hsToCss <$> css , writerTemplate = Just t }
    LBS.writeFile out f

  pdf %> \out -> do
    imgs <- getImages 
    plots <- getPlots
    diagrams <- getDiagrams
    drawings <- getDrawings
    need $ imgs
        <> view (mapping hsToPng) plots
        <> view (mapping hsToSvg) diagrams
        <> view (mapping addPngExt) drawings
    x <- getDirectoryFiles "" $ mdwn <> meta
    y <- mapM readFile' x
    f <- liftIO . runIOorExplode $ do
      p <- readMarkdown markdownReaderOptions $ Text.pack . join $ y
      makePDFLaTeX p
    LBS.writeFile out f

  beamer %> \out -> do
    imgs <- getImages 
    plots <- getPlots
    diagrams <- getDiagrams
    drawings <- getDrawings
    need $ imgs
        <> view (mapping hsToPng) plots
        <> view (mapping hsToSvg) diagrams
        <> view (mapping addPngExt) drawings
    x <- getDirectoryFiles "" $ mdwn <> meta
    y <- mapM readFile' x 
    f <- liftIO . runIOorExplode $ do
      p <- readMarkdown markdownReaderOptions $ Text.pack . join $ y
      makePDFBeamer p
    LBS.writeFile out f

  phony "pdf" $ need [pdf]
  phony "beamer" $ need [beamer]
  phony "test" $ need [index] >> cmd browser index
