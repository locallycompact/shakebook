import Control.Arrow
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

site = "_build"
browser = "chromium"

styleFiles = ["css/*.css", "fonts/*.ttf"]
styleDeps = map (site </>) <$> getDirectoryFiles "" styleFiles

copy :: FilePattern -> Rules ()
copy pattern = site </> pattern %> uncurry copyFile' . (dropDirectory1 &&& id)

main :: IO ()
main = shakeArgs shakeOptions $ do
  let index = site </> "index.html"
  let pdf = site </> "book.pdf"
  let beamer = site </> "beamer.pdf"

  let meta = ["title.txt"]

  want [index]

  phony "clean" $ do
    putNormal $ "Cleaning files in " ++ site
    removeFilesAfter "." [site]

  mapM copy styleFiles

  index %> \out -> do
    ms <- getDirectoryFiles "" ["//*.md"]
    ss <- styleDeps
    need $ meta ++ ms ++ ss
    cmd "pandoc" (meta ++ ms) ["-o", out, "-c", "css/style.css", "-t", "html", "-s",
                               "--template", "resources/page.tmpl", "-f", "markdown",
                               "--standalone", "--toc", "--toc-depth=2",
                               "--highlight-style", "pygments", "--mathjax"]

  pdf %> \out -> do
    ms <- getDirectoryFiles "" ["//*.md"]
    need $ meta ++ ms
    cmd "pandoc" (meta ++ ms) ["-o", out, "--standalone", "--toc", "--toc-depth=2"]

  beamer %> \out -> do
    ms <- getDirectoryFiles "" ["//*.md"]
    need $ meta ++ ms
    cmd "pandoc" (meta ++ ms) ["-o", out, "--standalone", "-t", "beamer"]

  phony "pdf" $ need [pdf]

  phony "beamer" $ need [beamer]

  phony "test" $ do
    need [index]
    cmd browser index
