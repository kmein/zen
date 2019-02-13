{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Blessings
import Blessings.String
import Control.Parallel.Strategies (parMap, rpar)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, replace, unpack)
import qualified Data.Text.IO as Text
import Data.Tree (Tree(Node))
import Options.Applicative
import System.IO (hPutStrLn, stderr)
import Text.HTML.Scalpel
import Text.Pandoc hiding (Plain)
import Text.Regex (mkRegex)

parMapM :: (Applicative f) => (a -> f b) -> [a] -> f [b]
parMapM f = sequenceA . parMap rpar f

withZeno :: Scraper Text a -> Scraper Text a
withZeno = chroot ("div" @: [hasClass "zenoCOMain"])

htmlToMarkdown :: Zenoptions -> Text -> Text
htmlToMarkdown Zenoptions {..} text =
  either (error . show) id $
  runPure $
  writeMarkdown
    def
      { writerVariables = writerVariables def ++ metadata
      , writerTOCDepth = 5
      , writerWrapText = WrapNone
      } =<<
  readHtml def text
  where
    metadata = variable "title" title <> variable "author" author
    variable name = maybe mempty (pure . (,) name)

walkLinks :: Scraper Text a -> URL -> IO (Tree (URL, a))
walkLinks page root = do
  hPutStrLn stderr $ "scraping " <> pp (SGR [36] (Plain root))
  taggedSublinks <- scrapeURL root (liftA2 (,) sublinks page)
  case taggedSublinks of
    Nothing -> pure $ Node (root, undefined) []
    Just (links, pageText) -> Node (root, pageText) <$> mapM (walkLinks page) links

sublinks :: Scraper Text [URL]
sublinks =
  withZeno $
  chroots (tagSelector "li") $ unpack . ("http://www.zeno.org" <>) <$> attr "href" (tagSelector "a")

content :: Scraper Text Text
content = replace "href=\"/" "href=\"http://www.zeno.org/" <$> withZeno (html anySelector)

data Zenoptions = Zenoptions
  { url :: String
  , author :: Maybe String
  , title :: Maybe String
  }

zenoptions :: Parser Zenoptions
zenoptions = do
  url <- strArgument (help "root URL for scraping" <> metavar "URL")
  author <-
    optional $
    strOption (long "author" <> short 'a' <> help "author of the work" <> metavar "STRING")
  title <-
    optional $ strOption (long "title" <> short 't' <> help "title of the work" <> metavar "STRING")
  pure Zenoptions {..}

main :: IO ()
main = do
  options <- execParser options
  links <- walkLinks (htmlToMarkdown options <$> content) (url options)
  hPutStrLn stderr $ pp $ "scraping " <> SGR [32] "complete"
  traverse_ (\(_, text) -> Text.putStrLn text >> putChar '\n') links
  where
    options = info (zenoptions <**> helper) (fullDesc <> progDesc "Scrape zeno.org")
