-- Copyright (c) 2015 Christian Siefkes
--
-- See accompanying LICENSE file for licensing information.
--
-- |Show how English is pronounced.

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char
import Data.Function
import System.Environment

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import Paths_phoneng (getDataFileName)

-- |Main entry point.
main :: IO ()
main = do
    -- TODO initially treat each command-line arg as filename and process it
    phoneticDict <- readPhoneticDict
    args <- getArgs
    mapM_ processFile args

-- TODO implement correctly, returning dict contents
readPhoneticDict :: IO (Map Text Text)
readPhoneticDict = do
    phoneticDict <- getDataFileName "phonetic-dict.txt"
    putStrLn phoneticDict
    return Map.empty

processFile :: String -> IO ()
processFile file = do
    contents <- LT.readFile file
    mapM_ (T.putStrLn . pronounceLine) (strictLines contents)

-- |Convert a line of text into its pronunciation.
pronounceLine :: Text -> Text
pronounceLine = T.concat . map pronounceToken . tokenize

-- |Convert a text into a list of tokens. Each token is either a word (letter
-- sequence) or a non-word (sequence of other characters). Words that contain
-- an apostrophe in a typical position (e.g. "she'll" and "O'Neill") will be
-- preserved as a single token rather.
tokenize :: Text -> [Text]
tokenize line = consolidate tokens
  where
    tokens = T.groupBy ((==) `on` isLetter) line
    consolidate :: [Text] -> [Text]
    consolidate (a:b:c:rest)
      | apostrophe b, typicalPrefix a || typicalSuffix c
        = T.concat [a, b, c] : consolidate rest
    consolidate (t:rest) = t : consolidate rest
    consolidate []       = []
    apostrophe t         = T.length t == 1 && toLower (T.head t) `elem` "'’"
    typicalPrefix t      = T.length t == 1 && toLower (T.head t) `elem` "dlo"
    typicalSuffix t      =
        T.toLower t `elem` ["am", "d", "en", "er", "ll", "m", "re", "t", "ve"]

-- |Convert a token (word or non-word) into its pronunciation.
pronounceToken :: Text -> Text
-- TODO implement correctly
pronounceToken token | isLetter $ T.head token = '?' `T.cons` token
pronounceToken token = token

-- |Break a lazy text into a list of strict texts at newline chars.
strictLines :: LT.Text -> [Text]
strictLines = map (T.concat . LT.toChunks) . LT.lines
