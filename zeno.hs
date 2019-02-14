{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Blessings
import Blessings.String
import Control.Concurrent.Async
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Foldable (for_, traverse_)
import Data.List
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.ICU.Replace as ICU
import qualified Data.Text.IO as Text
import Data.Tree
import Options.Applicative
import System.Exit
import System.IO
import System.IO.Temp
import Text.HTML.Scalpel
import Text.Pandoc.App
import Text.Regex (mkRegex)

logInfo = hPutStrLn stderr . pp

zenoCSS :: String
zenoCSS = "a,abbr,acronym,address,applet,article,aside,audio,b,big,blockquote,body,canvas,caption,center,cite,code,dd,del,details,dfn,div,dl,dt,em,embed,fieldset,figcaption,figure,footer,form,h1,h2,h3,h4,h5,h6,header,hgroup,html,i,iframe,img,ins,kbd,label,legend,li,mark,menu,nav,object,ol,output,p,pre,q,ruby,s,samp,section,small,span,strike,strong,sub,summary,sup,table,tbody,td,tfoot,th,thead,time,tr,tt,u,ul,var,video{margin:0;padding:0;border:0;font:inherit}em,h6,i,p.fn{font-style:italic}article,aside,details,figcaption,figure,footer,header,hgroup,menu,nav,section{display:block}body{line-height:1;margin:.5em}ol,ul{list-style:none}blockquote,q{quotes:none}blockquote:after,blockquote:before,q:after,q:before{content:'';content:none}table{border-collapse:collapse;border-spacing:0}td{padding:.4em;vertical-align:top}div.pageBreak{page-break-after:always;font-size:1px;margin:0;padding:0}a.page{color:gray;font-size:80%;text-decoration:none;padding:0 .25em}br,p{font-size:100%;line-height:140%}.zenoUnderline,a.page:active,a.page:hover,span.zenoUnderline{text-decoration:underline}br{padding-bottom:11pt}br.zenoHZA{font-size:50%;line-height:70%;padding-bottom:5.5pt}col,colgroup,img.break,img.left,img.right,li,p{text-align:justify}h1,h2,h3{font-weight:700;text-align:center}h1{font-size:200%;line-height:280%}h2{font-size:160%;line-height:224%}h3{font-size:140%;line-height:196%}h4{font-size:120%;line-height:168%;font-weight:700;text-align:center}h5,h6{font-size:100%;line-height:140%;padding-top:3.3pt;padding-bottom:7.7pt;text-align:center;font-weight:700}p.fn,p.zenoCellCenter{text-align:center}p.zenoCellLeft{text-align:left}p.zenoCellRight{text-align:right}p.zenoLYm4n8{padding-left:2em;text-indent:-1em;text-align:left}p.zenoPC{text-align:center}p.zenoPLf0h40,p.zenoPLm0n0,p.zenoPLm0n4,p.zenoPLm0n8,p.zenoPLm12n0,p.zenoPLm12n12,p.zenoPLm12n16,p.zenoPLm12n8,p.zenoPLm16n16,p.zenoPLm16n20,p.zenoPLm2024,p.zenoPLm20n0,p.zenoPLm20n16,p.zenoPLm20n20,p.zenoPLm24n0{text-align:justify}p.zenoPLm0n4{padding-left:1em;text-indent:-1em}p.zenoPLm0n8{padding-left:2em;text-indent:-2em}p.zenoPLm12n0{padding-left:0;text-indent:3em}p.zenoPLm12n12{padding-left:3em}p.zenoPLm12n16{padding-left:4em;text-indent:-1em}p.zenoPLm12n8{padding-left:2em;text-indent:1em}p.zenoPLm16n0{padding-left:0;text-indent:4em;text-align:justify}p.zenoPLm16n16{padding-left:4em}p.zenoPLm16n20{padding-left:5em;text-indent:-1em}p.zenoPLm20n0{padding-left:0;text-indent:5em}p.zenoPLm20n16{padding-left:4em;text-indent:1em}p.zenoPLm20n20{padding-left:5em}p.zenoPLm2024{padding-left:6em;text-indent:-1em}p.zenoPLm24n0{padding-left:0;text-indent:6em}p.zenoPLm24n24{padding-left:6em;text-align:justify}p.zenoPLm2428,p.zenoPLm28n28{padding-left:7em;text-align:justify}p.zenoPLm2428{text-indent:-1em}p.zenoPLm32n32{padding-left:8em;text-align:justify}p.zenoPLm36n36{padding-left:9em;text-align:justify}p.zenoPLm4n0{padding-left:0;text-indent:1em;text-align:justify}p.zenoPLm4n12{padding-left:3em;text-indent:-2em;text-align:justify}p.zenoPLm4n16{padding-left:4em;text-indent:-3em;text-align:justify}p.zenoPLm4n8,p.zenoPLm8n12{text-indent:-1em;text-align:justify}p.zenoPLm4n4{padding-left:1em;text-align:justify}p.zenoPLm4n8{padding-left:2em}p.zenoPLm8n12{padding-left:3em}p.zenoPLm8n4{padding-left:1em;text-indent:1em;text-align:justify}p.zenoPLm8n8{padding-left:2em;text-align:justify}p.zenoPR,p.zenoPRf0h40{text-align:right}table,tbody,td,tr,ul{text-align:justify}b{font-weight:700}fnref{display:none}img.float{height:1em;display:inline}span.arial{font-family:Arial}span.hebrew{font-family:Times New Roman}"

withZeno :: Scraper Text a -> Scraper Text a
withZeno = chroot ("div" @: [hasClass "zenoCOMain"])

generateEPUB :: Zenoptions -> Text -> IO ()
generateEPUB Zenoptions {..} text = do
  logInfo $ "generating epub to " <> SGR [33] (Plain outputFile)
  htmlFile <- writeSystemTempFile "zeno.html" (Text.unpack text)
  cssFile <- writeSystemTempFile "zeno.css" zenoCSS
  convertWithOpts
    defaultOpts
      { optReader = Just "html"
      , optWriter = Just "epub"
      , optMetadata =
          optMetadata defaultOpts ++ [("author", author), ("title", title), ("lang", "de")]
      , optOutputFile = Just outputFile
      , optSelfContained = True
      , optCss = [cssFile]
      , optInputFiles = [htmlFile]
      }

linkTree :: URL -> IO (Tree URL)
linkTree url = do
  logInfo $ "discovering " <> SGR [36] (Plain url)
  maybeSublinks <- scrapeURL url sublinks
  case maybeSublinks of
    Nothing -> pure $ Node url []
    Just links -> Node url <$> mapConcurrently linkTree links
  where
    normalize = Text.unpack . ("http://www.zeno.org" <>)
    sublinks = withZeno $ liLinks <|> pLinks
    pLinks = map normalize <$> attrs "href" ("a" @: [noSection, hasClass "zenoTXLinkInt"])
    liLinks = map normalize <$> attrs "href" ("div" @: [hasClass "zenoTRNavBottom"] // "ul" // "li" // "a" @: [noSection])
    noSection = notP $ "href" @=~ mkRegex "#"

pageContent :: URL -> IO Text
pageContent url = do
  logInfo $ "downloading " <> SGR [36] (Plain url)
  maybeContent <- scrapeURL url page
  case maybeContent of
    Nothing -> pure mempty
    Just content ->
      pure .
      ICU.replaceAll "<h3 pp=\"no\">.*</h3>" "" .
      ICU.replaceAll "<(no)?script>.*</(no)?script>" "" .
      Text.replace "=\"/" "=\"http://www.zeno.org/" $
      content
  where
    page = withZeno $ html anySelector

fetchContents :: Tree URL -> IO [Text]
fetchContents urls = mapConcurrently pageContent (nub (flatten urls))

data Zenoptions = Zenoptions
  { url :: URL
  , author :: String
  , title :: String
  , outputFile :: FilePath
  }

zenoptions :: Parser Zenoptions
zenoptions = do
  url <- strArgument (help "root URL for scraping" <> metavar "URL")
  author <- strOption (long "author" <> short 'a' <> help "author of the work" <> metavar "STRING")
  title <- strOption (long "title" <> short 't' <> help "title of the work" <> metavar "STRING")
  outputFile <- strOption (long "output" <> short 'o' <> help "output file name" <> metavar "PATH")
  pure Zenoptions {..}

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  options@Zenoptions {..} <- execParser options
  links <- linkTree url
  pageContents <- fetchContents links
  generateEPUB options (Text.unlines pageContents)
  where
    options = info (zenoptions <**> helper) (fullDesc <> progDesc "Scrape zeno.org")
