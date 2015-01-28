-- Copyright (c) 2015 Christian Siefkes
--
-- See accompanying LICENSE file for licensing information.
--
-- |Show how English is pronounced.

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

-- |Main entry point.
main :: IO ()
main = do
    -- TODO initially treat each command-line arg as filename and process it
    args <- getArgs
    mapM_ processFile args

processFile :: String -> IO ()
processFile file = do
    contents <- LT.readFile file
    mapM_ (T.putStrLn . pronounceLine) (strictLines contents)

-- |Convert a line of text into its pronunciation.
pronounceLine :: Text -> Text
-- TODO convert into sequence of words (letter sequences) and non-words.
-- If a non-word is an apostrophe (' or ’) and the next word is one if
-- "am d en er ll m re t ve" or the previous one of "d L O", join them
pronounceLine = T.toLower

-- |Break a lazy text into a list of strict texts at newline chars.
strictLines :: LT.Text -> [Text]
strictLines = map (T.concat . LT.toChunks) . LT.lines
