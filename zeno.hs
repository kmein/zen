{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Blessings
import Blessings.Text
import Control.Concurrent.Async
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteStringChar
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
import Network.Curl.Download.Lazy
import Options.Applicative
import Say (sayErr)
import System.Exit
import System.IO.Temp
import Text.HTML.Scalpel
import Text.Pandoc.App

data Mode
  = GutenbergDE
  | Zeno
  deriving (Show)

root :: Mode -> URL
root Zeno = "http://www.zeno.org"
root GutenbergDE = "http://gutenberg.spiegel.de"

--
-- helpers
--
logInfo' = sayErr . pp

logInfo mode = logInfo' . (SGR [1] (Plain (Text.pack (show mode))) <>) . (" " <>)

blessURL :: URL -> Blessings Text
blessURL = SGR [36] . Plain . Text.pack

gutenb :: Selector
gutenb = "div" @: ["id" @= "gutenb"]

zenoCOMain :: Selector
zenoCOMain = "div" @: [hasClass "zenoCOMain"]

zenoCOHeader :: Selector
zenoCOHeader = "div" @: [hasClass "zenoCOHeader"]

extractTitlePageLink :: URL -> IO (Maybe URL)
extractTitlePageLink url = scrapeURL url $ (root GutenbergDE <>) <$> chroot gutenb (attr "href" "a")

--
-- links
--
extractLinks :: Mode -> URL -> IO (Tree URL)
extractLinks mode url = do
  logInfo mode $ "extracting links from " <> blessURL url
  maybeSublinks <- scrapeURL url (sublinks mode)
  case maybeSublinks of
    Nothing -> pure $ Node url []
    Just links -> Node url <$> mapConcurrently (extractLinks mode) links
  where
    sublinks GutenbergDE = map (root GutenbergDE <>) <$> chroot gutenb (attrs "href" "a")
    sublinks Zeno = map (root Zeno <>) <$> chroot zenoCOMain (liLinks <|> pLinks)
      where
        pLinks = attrs "href" ("a" @: [hasClass "zenoTXLinkInt"])
        liLinks = attrs "href" ("div" @: [hasClass "zenoTRNavBottom"] // "ul" // "li" // "a")

--
-- metadata
--
metadataWith :: String -> (Mode -> IO (Maybe Text)) -> Mode -> IO (Maybe Text)
metadataWith name extract mode = do
  let name' = Plain (Text.pack name)
  logInfo mode $ "extracting " <> name' <> " information"
  meta <- extract mode
  meta <$
    case meta of
      Nothing -> logInfo mode $ name' <> " not found"
      Just value -> logInfo mode $ name' <> " found: " <> SGR [33] (Plain value)

extractSource :: Mode -> URL -> IO (Maybe Text)
extractSource mode url = metadataWith "source" extract mode
  where
    extract Zeno = scrapeURL url (text ("div" @: [hasClass "zenoCOFooterLineRight"]))
    extract GutenbergDE =
      scrapeURL url $
      chroot ("div" @: ["id" @= "metadata"] // "table") $ do
        let row = texts ("tr" // "td")
        ["type", bookType] <- row
        ["booktitle", bookBooktitle] <- row
        ["author", bookAuthor] <- row
        ["year", bookYear] <- row
        ["publisher", bookPublisher] <- row
        ["address", bookAddress] <- row
        ["title", bookTitle] <- row
        ["pages", bookPages] <- row
        ["created", bookCreated] <- row
        ["sender", bookSender] <- row
        ["firstpub", bookFirstpub] <- row
        pure $
          bookAuthor <> ": " <> bookBooktitle <> ". " <> bookPublisher <> ", " <> bookAddress <>
          bookYear <>
          ", S. " <>
          bookPages

extractAuthor :: Mode -> URL -> IO (Maybe Text)
extractAuthor mode url = metadataWith "author" extract mode
  where
    extract Zeno = scrapeURL url (chroot zenoCOMain $ text "h1" <|> text "h3")
    extract GutenbergDE = do
      titlePage <- extractTitlePageLink url
      case titlePage of
        Nothing -> pure Nothing
        Just url' -> scrapeURL url' (chroot gutenb $ text ("h3" @: [hasClass "author"]))

extractTitle :: Mode -> URL -> IO (Maybe Text)
extractTitle mode url = metadataWith "title" extract mode
  where
    extract Zeno = scrapeURL url (chroot zenoCOMain $ text "h2")
    extract GutenbergDE = do
      titlePage <- extractTitlePageLink url
      case titlePage of
        Nothing -> pure Nothing
        Just url' -> scrapeURL url' (chroot gutenb $ text ("h2" @: [hasClass "title"]))

extractSubtitle :: Mode -> URL -> IO (Maybe Text)
extractSubtitle mode url = metadataWith "subtitle" extract mode
  where
    extract Zeno = pure Nothing
    extract GutenbergDE = do
      titlePage <- extractTitlePageLink url
      case titlePage of
        Nothing -> pure Nothing
        Just url' -> scrapeURL url' (chroot gutenb $ text ("h3" @: [hasClass "subtitle"]))

--
-- cover
--
extractBookCover :: Mode -> URL -> IO (Maybe URL)
extractBookCover mode url = do
  logInfo mode "extracting book cover"
  extract mode
  where
    extract GutenbergDE = do
      titlePage <- extractTitlePageLink url
      case titlePage of
        Nothing -> pure Nothing
        Just url' ->
          scrapeURL url' $ do
            base <- attr "href" "base"
            chroot gutenb $ (base <>) <$> attr "src" ("p" @: [hasClass "figure"] // "img")
    extract Zeno = scrapeURL url (bookCoverImage <|> authorCoverImage)
      where
        authorCoverImage =
          chroot zenoCOMain $
          Text.unpack .
          ICU.replace
            "/Literatur.images/I/(.*)\\.jpg.*"
            "http://images.zeno.org/Literatur/I/big/$1.jpg" <$>
          attr "src" ("div" @: [hasClass "zenoTXThumbRight"] // "img")
        bookCoverImage =
          chroot zenoCOHeader $
          Text.unpack . Text.replace "/0000c.jpg" "/0000.jpg" <$>
          attr "src" ("img" @: [hasClass "zenoPayload2ImageThumb"])

downloadBookCover :: Maybe URL -> IO (Maybe ByteString)
downloadBookCover Nothing = Nothing <$ logInfo' "book cover not found"
downloadBookCover (Just url) = do
  logInfo' $ "downloading " <> blessURL url
  either (const Nothing) Just <$> openLazyURI url

--
-- text
--
extractHTML :: Mode -> URL -> IO Text
extractHTML mode url = do
  logInfo mode $ "extracting text from " <> blessURL url
  extract mode
  where
    extract GutenbergDE =
      maybe mempty (\(base, text) -> Text.replace "src=\"" ("src=\"" <> base <> "/") text) <$>
      scrapeURL url ((,) <$> attr "href" "base" <*> innerHTML gutenb)
    extract Zeno =
      maybe
        mempty
        (ICU.replaceAll "<div class=\"zenoTRNavBottom\">.*</div>" "lorem ipsum" .
         Text.replace "Amazon.de Widgets" "" .
         Text.replace "=\"/" ("=\"" <> Text.pack (root Zeno) <> "/")) <$>
      scrapeURL url (innerHTML zenoCOMain)

linksToHTML :: Mode -> Tree URL -> IO [Text]
linksToHTML mode = mapConcurrently (extractHTML mode) . nub . flatten

--
-- epub
--
zenoCSS :: String
zenoCSS =
  "a,abbr,acronym,address,applet,article,aside,audio,b,big,blockquote,body,canvas,caption,center,cite,code,dd,del,details,dfn,div,dl,dt,em,embed,fieldset,figcaption,figure,footer,form,h1,h2,h3,h4,h5,h6,header,hgroup,html,i,iframe,img,ins,kbd,label,legend,li,mark,menu,nav,object,ol,output,p,pre,q,ruby,s,samp,section,small,span,strike,strong,sub,summary,sup,table,tbody,td,tfoot,th,thead,time,tr,tt,u,ul,var,video{margin:0;padding:0;border:0;font:inherit}em,h6,i,p.fn{font-style:italic}article,aside,details,figcaption,figure,footer,header,hgroup,menu,nav,section{display:block}body{line-height:1;margin:.5em}ol,ul{list-style:none}blockquote,q{quotes:none}blockquote:after,blockquote:before,q:after,q:before{content:'';content:none}table{border-collapse:collapse;border-spacing:0}td{padding:.4em;vertical-align:top}div.pageBreak{page-break-after:always;font-size:1px;margin:0;padding:0}a.page{color:gray;font-size:80%;text-decoration:none;padding:0 .25em}br,p{font-size:100%;line-height:140%}.zenoUnderline,a.page:active,a.page:hover,span.zenoUnderline{text-decoration:underline}br{padding-bottom:11pt}br.zenoHZA{font-size:50%;line-height:70%;padding-bottom:5.5pt}col,colgroup,img.break,img.left,img.right,li,p{text-align:justify}h1,h2,h3{font-weight:700;text-align:center}h1{font-size:200%;line-height:280%}h2{font-size:160%;line-height:224%}h3{font-size:140%;line-height:196%}h4{font-size:120%;line-height:168%;font-weight:700;text-align:center}h5,h6{font-size:100%;line-height:140%;padding-top:3.3pt;padding-bottom:7.7pt;text-align:center;font-weight:700}p.fn,p.zenoCellCenter{text-align:center}p.zenoCellLeft{text-align:left}p.zenoCellRight{text-align:right}p.zenoLYm4n8{padding-left:2em;text-indent:-1em;text-align:left}p.zenoPC{text-align:center}p.zenoPLf0h40,p.zenoPLm0n0,p.zenoPLm0n4,p.zenoPLm0n8,p.zenoPLm12n0,p.zenoPLm12n12,p.zenoPLm12n16,p.zenoPLm12n8,p.zenoPLm16n16,p.zenoPLm16n20,p.zenoPLm2024,p.zenoPLm20n0,p.zenoPLm20n16,p.zenoPLm20n20,p.zenoPLm24n0{text-align:justify}p.zenoPLm0n4{padding-left:1em;text-indent:-1em}p.zenoPLm0n8{padding-left:2em;text-indent:-2em}p.zenoPLm12n0{padding-left:0;text-indent:3em}p.zenoPLm12n12{padding-left:3em}p.zenoPLm12n16{padding-left:4em;text-indent:-1em}p.zenoPLm12n8{padding-left:2em;text-indent:1em}p.zenoPLm16n0{padding-left:0;text-indent:4em;text-align:justify}p.zenoPLm16n16{padding-left:4em}p.zenoPLm16n20{padding-left:5em;text-indent:-1em}p.zenoPLm20n0{padding-left:0;text-indent:5em}p.zenoPLm20n16{padding-left:4em;text-indent:1em}p.zenoPLm20n20{padding-left:5em}p.zenoPLm2024{padding-left:6em;text-indent:-1em}p.zenoPLm24n0{padding-left:0;text-indent:6em}p.zenoPLm24n24{padding-left:6em;text-align:justify}p.zenoPLm2428,p.zenoPLm28n28{padding-left:7em;text-align:justify}p.zenoPLm2428{text-indent:-1em}p.zenoPLm32n32{padding-left:8em;text-align:justify}p.zenoPLm36n36{padding-left:9em;text-align:justify}p.zenoPLm4n0{padding-left:0;text-indent:1em;text-align:justify}p.zenoPLm4n12{padding-left:3em;text-indent:-2em;text-align:justify}p.zenoPLm4n16{padding-left:4em;text-indent:-3em;text-align:justify}p.zenoPLm4n8,p.zenoPLm8n12{text-indent:-1em;text-align:justify}p.zenoPLm4n4{padding-left:1em;text-align:justify}p.zenoPLm4n8{padding-left:2em}p.zenoPLm8n12{padding-left:3em}p.zenoPLm8n4{padding-left:1em;text-indent:1em;text-align:justify}p.zenoPLm8n8{padding-left:2em;text-align:justify}p.zenoPR,p.zenoPRf0h40{text-align:right}table,tbody,td,tr,ul{text-align:justify}b{font-weight:700}fnref{display:none}img.float{height:1em;display:inline}span.arial{font-family:Arial}span.hebrew{font-family:Times New Roman}"

data Metadata = Metadata
  { author :: Maybe Text
  , title :: Maybe Text
  , subtitle :: Maybe Text
  , source :: Maybe Text
  , cover :: Maybe ByteString
  }

extractMetadata :: Mode -> URL -> IO Metadata
extractMetadata mode url =
  runConcurrently $ do
    author <- Concurrently (extractAuthor mode url)
    title <- Concurrently (extractTitle mode url)
    subtitle <- Concurrently (extractSubtitle mode url)
    source <- Concurrently (extractSource mode url)
    cover <- Concurrently (downloadBookCover =<< extractBookCover mode url)
    pure Metadata {..}

generateEPUB :: Metadata -> Zenoptions -> Text -> IO ()
generateEPUB Metadata {..} Zenoptions {..} text = do
  logInfo' $
    "generating " <> Plain (Text.pack outputType) <> " to " <>
    SGR [33] (Plain (Text.pack outputFile))
  (htmlFile, cssFile) <-
    writeSystemTempFile "zeno.html" (Text.unpack text) `concurrently`
    writeSystemTempFile "zeno.css" zenoCSS
  coverFile <-
    case cover of
      Nothing -> pure Nothing
      Just bytes ->
        let path =
              Text.unpack $ "/tmp/" <> fromMaybe mempty author <> fromMaybe mempty title <> ".jpg"
         in Just path <$ ByteString.writeFile path bytes
  convertWithOpts
    defaultOpts
      { optReader = Just "html"
      , optWriter = Just outputType
      , optMetadata =
          optMetadata defaultOpts <> [("lang", "de")] <> variable "author" author <>
          variable "title" title' <>
          variable "subtitle" subtitle <>
          variable "date" source
      , optOutputFile = Just outputFile
      , optSelfContained = True
      , optCss = [cssFile]
      , optInputFiles = [htmlFile]
      , optEpubCoverImage = coverFile
      }
  where
    variable name = maybe mempty (pure . (,) name . Text.unpack)
    title' = title <|> Just "Werke"

--
-- cli
--
data Zenoptions = Zenoptions
  { url :: URL
  , outputFile :: FilePath
  , outputType :: String
  }

guessMode :: URL -> Maybe Mode
guessMode url
  | url `startsWith` root GutenbergDE = Just GutenbergDE
  | url `startsWith` root Zeno = Just Zeno
  | otherwise = Nothing
  where
    xs `startsWith` ys = take (length ys) xs == ys

zenoptions :: Parser Zenoptions
zenoptions = do
  url <- strArgument (help "root URL for scraping" <> metavar "URL")
  outputFile <- strOption (long "output" <> short 'o' <> help "output file name" <> metavar "PATH")
  outputType <-
    strOption
      (long "type" <> short 't' <> help "output file type (from pandoc --list-output-formats)" <>
       metavar "TYPE" <>
       value "epub2" <>
       showDefault)
  pure Zenoptions {..}

main :: IO ()
main = do
  options@Zenoptions {..} <- execParser options
  case guessMode url of
    Nothing -> sayErr $ pp $ SGR [31] $ "Site " <> blessURL url <> " not supported."
    Just mode -> do
      (metadata, pageContents) <-
        extractMetadata mode url `concurrently` (linksToHTML mode =<< extractLinks mode url)
      generateEPUB metadata options (Text.unlines pageContents)
  where
    options = info (zenoptions <**> helper) (fullDesc <> progDesc "Download public domain ebooks")
