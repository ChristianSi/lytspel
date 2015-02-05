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
import PhonEng (PosTag, textToPos)

---- Data types and related functions ----

data PhoneticEntry
    = SimpleEntry !Text
    | RedirectEntry !Text
    | TaggedEntry !(Map PosTag Text)

type PhoneticDict = Map (CI Text) PhoneticEntry

-- |Main entry point.
main :: IO ()
main = do
    -- TODO initially treat each command-line arg as filename and process it
    pd <- readPhoneticDict
    args <- getArgs
    mapM_ (processFile pd) args

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
  | length parts == 2 = if ">" `T.isPrefixOf` last parts
        then Map.insert key (RedirectEntry . T.strip . T.tail . last $ parts) pd
        else Map.insert key (SimpleEntry . T.strip . last $ parts) pd
  | length parts > 2  = insertTaggedEntry pd parts
  | otherwise         = error $ concat [
        "pronounce:addEntryFromParts: Not a valid dict entry: '",
        T.unpack $ T.concat parts, "'"]
  where key = CI.mk . T.strip . head $ parts

-- |Insert a TaggedEntry from a line already split in parts at ":".
-- For example, the split entry ["item/av", "ï°tem; n", "ï°t'm; v", "ï°t'm"]
-- will be converted into a mapping from "item" to the map
-- {Av -> "ï°tem", N -> "ï°t'm", V -> "ï°t'm"}
insertTaggedEntry :: PhoneticDict -> [Text] -> PhoneticDict
insertTaggedEntry pd parts =
    Map.insert (CI.mk $ head splitParts) (TaggedEntry tagMap) pd
  where
    splitParts :: [Text]
    splitParts = map T.strip $ T.splitOn "/" (head parts) ++
                               concatMap (T.splitOn ";") (tail parts)
    tagMap :: Map PosTag Text
    tagMap = makeTagMap $ tail splitParts
    makeTagMap :: [Text] -> Map PosTag Text
    makeTagMap (key:val:rest) = Map.insert (textToPos key) val (makeTagMap rest)
    makeTagMap []  = Map.empty
    makeTagMap [_] = error $ concat [
        "pronounce:insertTaggedEntry: Not a valid dict entry: '",
        T.unpack $ T.intercalate ":" parts, "'"]

processFile :: PhoneticDict -> String -> IO ()
processFile pd file = do
    contents <- LT.readFile file
    mapM_ (T.putStrLn . pronounceLine pd) (strictLines contents)

-- |Convert a line of text into its pronunciation.
pronounceLine :: PhoneticDict -> Text -> Text
pronounceLine pd = T.concat . map (pronounceToken pd) . tokenize

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
pronounceToken :: PhoneticDict -> Text -> Text
pronounceToken pd token | isLetter $ T.head token =
    pronounceWord pd (Map.lookup (CI.mk token) pd) token
pronounceToken _ token = token

pronounceWord :: PhoneticDict -> Maybe PhoneticEntry -> Text -> Text
pronounceWord _ (Just (SimpleEntry pron)) _ = pron
pronounceWord pd (Just (RedirectEntry redirect)) _ = pronounceToken pd redirect
-- TODO handle TaggedEntry correctly
pronounceWord _ (Just (TaggedEntry _)) token = "!t!" `T.append` token
pronounceWord _ Nothing token = '?' `T.cons` token

-- |Break a lazy text into a list of strict texts at newline chars.
strictLines :: LT.Text -> [Text]
strictLines = map (T.concat . LT.toChunks) . LT.lines
