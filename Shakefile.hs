import Control.Applicative
import Control.Monad
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

site    = "public"
browser = "chromium"

css   = ["css/*.css"]
fonts = ["fonts/*.ttf"]
imgs  = ["img/*.png"]
js    = ["js/*.js"]
mdwn  = ["notes//*.md"]

htemplate = "resources/page.tmpl"

tocOpts = ["--toc", "--toc-depth=2"]

getCSSFiles      = getDirectoryFiles "" css
getFonts         = getDirectoryFiles "" fonts
getImages        = getDirectoryFiles "" imgs
getJSFiles       = getDirectoryFiles "" js
getMarkdownFiles = getDirectoryFiles "" mdwn

supportingFile = flip copyFile' <*> dropDirectory1

main :: IO ()
main = shakeArgs shakeOptions $ do
  let index  = site </> "index.html"
  let pdf    = site </> "book.pdf"
  let beamer = site </> "beamer.pdf"

  let meta = ["meta.txt"]

  want [index]

  phony "clean" $ do
    putNormal $ "Cleaning files in " ++ site
    removeFilesAfter "." [site]

  let supports = map (site </>) $ css ++ fonts ++ imgs ++ js

  forM supports $ flip (%>) supportingFile

  index %> \out -> do
    css   <- getCSSFiles
    fonts <- getFonts
    imgs  <- getImages
    js    <- getJSFiles
    mdwn  <- getMarkdownFiles
    let cssOpts = css >>= (\x -> ["-c", x])
    need $ (site </>) <$> (css ++ fonts ++ imgs ++ js)
    need $ meta ++ mdwn
    cmd "pandoc" (meta ++ mdwn) $ ["-o", out, "-s", "--template", htemplate,
                                   "-t", "html", "-f", "markdown",
                                   "--highlight-style", "pygments",
                                   "--mathjax"] ++ cssOpts ++ tocOpts

  pdf %> \out -> do
    mdwn <- getMarkdownFiles
    need $ meta ++ mdwn
    cmd "pandoc" (meta ++ mdwn) $ ["-o", out, "-s"] ++ tocOpts

  beamer %> \out -> do
    mdwn <- getMarkdownFiles
    need $ meta ++ mdwn
    cmd "pandoc" (meta ++ mdwn) ["-o", out, "-s", "-t", "beamer"]

  phony "pdf" $ need [pdf]

  phony "beamer" $ need [beamer]

  phony "test" $ do
    need [index]
    cmd browser index
