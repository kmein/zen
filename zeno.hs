#!/usr/bin/env nix-shell
#! nix-shell -i runghc -p "ghc.withPackages (ps: [ ps.scalpel ps.regex-compat ])"
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Text.Regex (mkRegex)
import Control.Monad

withZeno :: Scraper String a -> Scraper String a
withZeno = chroot ("div" @: [hasClass "zenoCOMain"])

hasAttribute :: AttributePredicate
hasAttribute = AnyAttribute @=~ mkRegex ".*"

sublinks :: Scraper String [URL]
sublinks =
  withZeno $
  chroots (tagSelector "li") $
  ("http://www.zeno.org" <>) <$> attr "href" (tagSelector "a")

paragraphs :: Scraper String [String]
paragraphs = withZeno $ htmls $ "p" @: [notP hasAttribute]

specialParagraphs :: Scraper String [String]
specialParagraphs = withZeno $ htmls $ "p" @: [hasAttribute]

data Page = Page
  { url :: URL
  , title :: String
  , paragraphs :: [String]
  , specialParagraphs :: [String]
  }

main :: IO ()
main = do
  maybeLinks <- scrapeURL "http://www.zeno.org/Literatur/M/George,+Stefan" sublinks
  case maybeLinks of
    Nothing -> print "Nothing found"
    Just links -> forM_ links $ \link -> do
      ps <- scrapeURL link ((,) <$> paragraphs <*> specialParagraphs)
      maybe (return ()) print ps
