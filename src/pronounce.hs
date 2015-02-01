-- Copyright (c) 2015 Christian Siefkes
--
-- See accompanying LICENSE file for licensing information.
--
-- |Show how English is pronounced.

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char
import Data.Function
import Data.List
import System.Environment

import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import Paths_phoneng (getDataFileName)
import PhonEng (PosTag)

---- Data types and related functions ----

data PhoneticEntry
    =  SimpleEntry { pePron :: Text}
    | RedirectEntry { peRedirect :: Text }
    | TaggedEntry { peTagMap :: Map PosTag Text }

type PhoneticDict = Map (CI Text) PhoneticEntry

-- |Main entry point.
main :: IO ()
main = do
    -- TODO initially treat each command-line arg as filename and process it
    phoneticDict <- readPhoneticDict
    -- TODO just for testing
    T.putStrLn $ T.pack (show $ Map.size phoneticDict) `T.append` " entries in dict"
    args <- getArgs
    mapM_ processFile args

readPhoneticDict :: IO PhoneticDict
readPhoneticDict = do
    dictFile <- getDataFileName "phonetic-dict.txt"
    contents <- T.readFile dictFile
    return $ foldl' addDictEntry Map.empty $ T.lines contents
  where
    addDictEntry :: PhoneticDict -> Text -> PhoneticDict
    addDictEntry pd line = addEntryFromParts pd $ T.splitOn ":" line

-- Add a dictionary entry from a line already split at ":".
addEntryFromParts :: PhoneticDict -> [Text] -> PhoneticDict
addEntryFromParts pd parts
  | length parts == 2 = Map.insert (CI.mk $ T.strip $ head parts)
                                   (SimpleEntry $ T.strip $ last parts) pd
    -- TODO create TaggedEntry
  | length parts > 2 = Map.insert (CI.mk "dummy") (SimpleEntry "dummy") pd
  | otherwise        = error $ concat [
        "pronounce:addEntryFromParts: not a valid dict entry: '",
        T.unpack $ T.concat parts, "'"]

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
