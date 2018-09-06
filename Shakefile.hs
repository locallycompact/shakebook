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

staticFileDeploy :: [FilePattern]
staticFileDeploy = view (mapping verbatim) $ join [fonts, imgs, js]

staticFileDeployRules :: Rules ()
staticFileDeployRules = forM_ staticFileDeploy (%> copyVerbatim)

--- CSS ------------------------------------------------------------------------

hsCssSrc :: [FilePattern]
hsCssSrc = ["css/*.hs"]

getCSSFiles :: Action [FilePath]
getCSSFiles = getDirectoryFiles "" hsCssSrc

hsToCss :: Iso' FilePath FilePath
hsToCss = iso (-<.> ".css") (-<.> ".hs")

hsCssResult :: [FilePattern]
hsCssResult = view (mapping hsToCss) hsCssSrc

hsCssDeploy :: [FilePattern]
hsCssDeploy = view (mapping verbatim) hsCssResult

compileCss :: FilePath -> Action ()
compileCss x = do
  let src = (view . from) hsToCss x
  need [src]
  Stdout z <- command [] src []
  writeFile' x z

cssCompileRules :: Rules ()
cssCompileRules = forM_ hsCssResult (%> compileCss)

cssDeployRules :: Rules ()
cssDeployRules = forM_ hsCssDeploy (%> copyVerbatim)

--- R Plots --------------------------------------------------------------------

plots :: [FilePattern]
plots = ["plots/*.hs"]

getPlots :: Action [FilePath]
getPlots = getDirectoryFiles "" plots

plotToPng :: Iso' FilePath FilePath
plotToPng = iso (-<.> ".png") (-<.> ".hs")

plotResult :: [FilePattern]
plotResult = view (mapping plotToPng) plots

plotDeploy :: [FilePattern]
plotDeploy = view (mapping verbatim) plotResult

compilePlot :: FilePath -> Action ()
compilePlot x = do
  let src = (view . from) plotToPng x
  need [src]
  command [] src ["-o", x] 

plotCompileRules :: Rules ()
plotCompileRules = forM_ plotResult (%> compilePlot)

plotDeployRules :: Rules ()
plotDeployRules = forM_ plotDeploy (%> copyVerbatim)

--- Diagrams -------------------------------------------------------------------

diagrams :: [FilePattern]
diagrams = ["diagrams/*.hs"]

diagramToSvg :: Iso' FilePath FilePath
diagramToSvg = iso (-<.> ".svg") (-<.> ".hs")

getDiagrams :: Action [FilePath]
getDiagrams = getDirectoryFiles "" diagrams

diagramResult :: [FilePattern]
diagramResult = view (mapping diagramToSvg) diagrams

diagramDeploy :: [FilePattern]
diagramDeploy = view (mapping verbatim) diagramResult

compileDiagram :: FilePath -> Action ()
compileDiagram x = do
  let src = (view . from) diagramToSvg x
  need [src]
  command [] src ["-o", x, "-w", "200", "-h", "200"]

diagramCompileRules :: Rules ()
diagramCompileRules = forM_ diagramResult (%> compileDiagram)

diagramDeployRules :: Rules ()
diagramDeployRules = forM_ diagramDeploy (%> copyVerbatim)

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
  cssCompileRules
  cssDeployRules
  plotCompileRules
  plotDeployRules
  diagramCompileRules
  diagramDeployRules

  index %> \out -> do
    css   <- getCSSFiles
    fonts <- getFonts
    imgs  <- getImages
    js    <- getJSFiles
    plots <- getPlots
    diagrams <- getDiagrams
    need $ view (mapping verbatim) $ join [fonts, imgs, js]
    need $ view (mapping hsToCss) css
    need $ view (mapping plotToPng) plots
    need $ view (mapping diagramToSvg) diagrams
    x <- getDirectoryFiles "" $ mdwn <> meta
    y <- mapM readFile' x
    t <- readFile' htemplate
    f <- makeHTML5Page (Text.pack . join $ y)
                       html5WriterOptions { writerVariables = ("css", ) . view hsToCss <$> css , writerTemplate = Just t }
    LBS.writeFile out f

  pdf %> \out -> do
    plots <- getPlots
    diagrams <- getDiagrams
    need $ view (mapping plotToPng) plots
    need $ view (mapping diagramToSvg) diagrams
    x <- getDirectoryFiles "" $ mdwn <> meta
    y <- mapM readFile' x
    f <- liftIO . runIOorExplode $ do
      p <- readMarkdown markdownReaderOptions $ Text.pack . join $ y
      makePDFLaTeX p
    LBS.writeFile out f

  beamer %> \out -> do
    plots <- getPlots
    diagrams <- getDiagrams
    need $ view (mapping plotToPng) plots
    need $ view (mapping diagramToSvg) diagrams
    x <- getDirectoryFiles "" $ mdwn <> meta
    y <- mapM readFile' x 
    f <- liftIO . runIOorExplode $ do
      p <- readMarkdown markdownReaderOptions $ Text.pack . join $ y
      makePDFBeamer p
    LBS.writeFile out f

  phony "pdf" $ need [pdf]
  phony "beamer" $ need [beamer]
  phony "test" $ need [index] >> cmd browser index
