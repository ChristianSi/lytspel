-- Copyright (c) 2015 Christian Siefkes
--
-- See accompanying LICENSE file for licensing information.
--
-- |Shared code for the phoneng tools.

{-# LANGUAGE OverloadedStrings #-}

module PhonEng
    ( PosTag(..)
    , posToText
    , textToPos
    , isVowel
    , isVowelButNotSchwa
    , str
    ) where

import Data.Char
import Data.Maybe
import Text.Read

import qualified Data.Text as T
import Data.Text (Text)

---- Data types and related functions ----

-- |POS (part-of-speech) markers as used in Moby.
data PosTag
    = Aj   -- ^Adjective
    | Av   -- ^Adverb
    | Inj  -- ^Interjection
    | N    -- ^Noun
    | Prp  -- ^Preposition
    | V    -- ^Verb
    deriving (Eq, Ord, Read, Show)

-- Convert a text such as "n" or "aj" into a 'PosTag'.
-- Throws an error if the text doesn't correspond to a 'PosTag'.
textToPos :: Text -> PosTag
textToPos "" = error "PhonEng.textToPos: Empty POS tag"
textToPos t  = fromMaybe errorMsg result
  where
    result = readMaybe $ toUpper (T.head t) : T.unpack (T.tail t)
    errorMsg = error $ "PhonEng.textToPos: Not a valid POS tag: " ++ T.unpack t

posToText :: PosTag -> Text
posToText = T.toLower . T.pack . show

---- Utility functions ----

isVowel :: Char -> Bool
isVowel '\'' = True
isVowel ch   = isVowelButNotSchwa ch

isVowelButNotSchwa :: Char -> Bool
isVowelButNotSchwa ch = ch `elem` str "aeiouäëïöüáéóú"

-- |Allows writing string literals without explicit type annotation (in
-- case ghc complains about ambiguity).
str :: String -> String
str = id
