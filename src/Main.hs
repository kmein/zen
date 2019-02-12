{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Data.Text (Text)
import Data.Tree
import Text.HTML.Scalpel

data Page = Page
  { url :: URL
  , body :: Text
  } deriving (Show)

type Book = Tree Page

book :: Scraper String Book
book =
  chroot ("div" @: [hasClass "zenoCOMain"]) $ do
    _
    _

scrapeBook :: IO (Maybe Book)
scrapeBook = scrapeURL "http://www.zeno.org/Literatur/M/George,+Stefan/Gesamtausgabe+der+Werke/Die+Fibel.+Auswahl+Erster+Verse" book

main :: IO ()
main = print =<< scrapeBook
