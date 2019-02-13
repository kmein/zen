{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Parallel.Strategies (parMap, rpar)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, replace, unpack)
import qualified Data.Text.IO as Text
import Data.Tree (Tree(Node))
import Options.Applicative
import System.IO (hPutStrLn, stderr)
import Text.HTML.Scalpel
import Text.Pandoc
import Text.Regex (mkRegex)

parMapM :: (Applicative f) => (a -> f b) -> [a] -> f [b]
parMapM f = sequenceA . parMap rpar f

withZeno :: Scraper Text a -> Scraper Text a
withZeno = chroot ("div" @: [hasClass "zenoCOMain"])

htmlToMarkdown :: Text -> Text
htmlToMarkdown text = either (error . show) id $ runPure $ writeMarkdown def =<< readHtml def text

walkLinks :: Scraper Text a -> URL -> IO (Tree (URL, a))
walkLinks page root = do
  hPutStrLn stderr $ "scraping " <> root
  taggedSublinks <- scrapeURL root (liftA2 (,) sublinks page)
  case taggedSublinks of
    Nothing -> pure $ Node (root, undefined) []
    Just (links, pageText) -> Node (root, pageText) <$> mapM (walkLinks page) links

sublinks :: Scraper Text [URL]
sublinks =
  withZeno $
  chroots (tagSelector "li") $ unpack . ("http://www.zeno.org" <>) <$> attr "href" (tagSelector "a")

content :: Scraper Text Text
content =
  replace "href=\"/" "href=\"http://www.zeno.org/" .
  replace "href=\"/Literatur/I" "href=\"http://images.zeno.org/Literatur/I/big" <$>
  withZeno (html anySelector)

newtype Zenoptions = Zenoptions
  { rootURL :: String
  -- , outputDirectory :: FilePath
  }

zenoptions :: Parser Zenoptions
zenoptions = do
  rootURL <- strArgument (help "Scraping root URL" <> metavar "URL")
  -- outputDirectory <-
  --   strOption (help "Directory to scrape to" <> long "output" <> short 'o' <> metavar "DIRECTORY")
  pure Zenoptions {..}

-- writeContent :: FilePath -> (Text, Text) -> IO ()
-- writeContent directory (path, text)
--   | Text.null text = createDirectory path'
--   | Text.null path = Text.writeFile (directory </> "index.md") text
--   | otherwise = Text.writeFile (path' <> ".md") text
--   where
--     path' = directory </> Text.unpack path
main :: IO ()
main = do
  Zenoptions {..} <- execParser options
  links <- walkLinks (htmlToMarkdown <$> content) rootURL
  -- putStrLn $ drawTree $ fmap show links
  -- outputDirectoryExists <- doesDirectoryExist outputDirectory
  -- unless outputDirectoryExists $ createDirectory outputDirectory
  --traverse_ (writeContent outputDirectory) links
  traverse_ (\(_, text) -> Text.putStrLn text >> putChar '\n') links
  --"http://www.zeno.org/Literatur/M/Trakl,+Georg"
  --"http://www.zeno.org/Literatur/M/George,+Stefan/Gesamtausgabe+der+Werke/Hymnen,+Pilgerfahrten,+Algabal/Pilgerfahrten"
  --"http://www.zeno.org/Literatur/M/George,+Stefan/Gesamtausgabe+der+Werke/Hymnen,+Pilgerfahrten,+Algabal"
  where
    options = info (zenoptions <**> helper) (fullDesc <> progDesc "Scrape zeno.org")
