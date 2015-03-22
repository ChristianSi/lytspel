-- Copyright (c) 2015 Christian Siefkes
--
-- See accompanying LICENSE file for licensing information.
--
-- |Show how English is pronounced.

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import System.Environment

import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import NLP.Corpora.Conll (Tag)
import NLP.POS (defaultTagger, tagTokens)
import NLP.Types (POSTagger(posSplitter, posTokenizer))
import qualified NLP.Types.Tree as Tree

import Paths_phoneng (getDataFileName)
import PhonEng (PosTag(Aj, Av, N, Prp, V), textToPos, isVowelButNotSchwa)

---- Data types and related functions ----

data PhoneticEntry
    = SimpleEntry !Text
    | RedirectEntry !Text
    | TaggedEntry !(Map PosTag Text)

type CISet        = Set (CI Text)
type PhoneticDict = Map (CI Text) PhoneticEntry

data State = State
  { stPD             :: PhoneticDict
    -- |Set of word whose pronounciation depends on their POS tag
  , stAmbiguousWords :: CISet
    -- |The POS tagger is loaded on demand
  , stTagger         :: Maybe (POSTagger Tag)
  }

-- |Main entry point.
main :: IO ()
main = do
    pd <- readPhoneticDict
    let state = State {stPD = pd,
                       stAmbiguousWords = mkAmbiguousWords pd,
                       stTagger = Nothing}
    args <- getArgs
    -- TODO initially treat each command-line arg as filename and process it
    foldM_ processFile state args
  where
    mkAmbiguousWords :: PhoneticDict -> CISet
    mkAmbiguousWords = Map.foldrWithKey insertTaggedWords Set.empty
    insertTaggedWords :: CI Text -> PhoneticEntry -> CISet -> CISet
    insertTaggedWords word (TaggedEntry _) set = Set.insert word set
    insertTaggedWords _ _ set = set

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
  | otherwise         = error $ concat ["addEntryFromParts: ",
        "Not a valid dict entry: '", T.unpack $ T.concat parts, "'"]
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
    makeTagMap [_] = error $ concat ["insertTaggedEntry: ",
        "Not a valid dict entry: '", T.unpack $ T.intercalate ":" parts, "'"]

processFile :: State -> String -> IO State
processFile state file = do
    contents <- LT.readFile file
    foldM pronounceLine state $ strictLines contents

-- |Break a lazy text into a list of strict texts at newline chars.
strictLines :: LT.Text -> [Text]
strictLines = map (T.concat . LT.toChunks) . LT.lines

-- |Convert a line of text into its pronounciation.
-- If one of the tokens in the line is ambiguous, the whole line is
-- POS-tagged to determine the correct pronounciation.
pronounceLine :: State -> Text -> IO State
pronounceLine state line = if any ambiguous tokens
    then do
        (result, state') <- pronounceAmbiguousLine state line
        T.putStrLn result
        return state'
    else do
        T.putStrLn . T.concat . map (pronounceToken $ stPD state) $ tokens
        return state
  where
    tokens :: [Text]
    tokens = tokenize line
    ambiguous token = Set.member (CI.mk token) $ stAmbiguousWords state

-- |Convert a text into a list of tokens. Each token is either a word (letter
-- sequence) or a non-word (sequence of other characters). Words that contain
-- an apostrophe in a typical position (e.g. "she'll", "O'Neill, author's")
-- will be preserved as a single token rather.
tokenize :: Text -> [Text]
tokenize line = consolidate tokens
  where
    tokens = sepWordsFromNonWords line
    consolidate :: [Text] -> [Text]
    consolidate (a:b:c:rest)
      | apostrophe b, typicalPrefix a || typicalSuffix c
        = T.concat [a, "'", c] : consolidate rest
    consolidate (t:rest) = t : consolidate rest
    consolidate []       = []
    apostrophe t         = T.length t == 1 && toLower (T.head t) `elem` "'’"
    typicalPrefix t      = T.length t == 1 && toLower (T.head t) `elem` "dlo"
    typicalSuffix t      = T.toLower t `elem`
                      ["am", "d", "en", "er", "ll", "m", "re", "s", "t", "ve"]

-- |Split a text into a list of words (letter sequences) and non-words
-- (sequences of other characters).
sepWordsFromNonWords :: Text -> [Text]
sepWordsFromNonWords = T.groupBy ((==) `on` isLetter)

pronounceAmbiguousLine :: State -> Text -> IO (Text, State)
pronounceAmbiguousLine state line
  | Just tagger <- stTagger state = do
        let posTokens = map ((Tree.Sent . fixMistokenizations . Tree.tokens) .
                             posTokenizer tagger) sentences
            sentences = posSplitter tagger $ normApo line
            tagged = Tree.tsConcat $ tagTokens tagger posTokens
        return (pronounceTaggedLine pd ourTokens tagged, state)
  | otherwise = do
        tagger <- defaultTagger
        pronounceAmbiguousLine state {stTagger = Just tagger} line
  where
    pd = stPD state
    ourTokens :: [Text]
    ourTokens = tokenize line
    fixMistokenizations :: [Tree.Token] ->  [Tree.Token]
    fixMistokenizations = concatMap $ map Tree.Token . splitPunct . Tree.showTok

-- |Replace typographic apostrophes by plain ones.
normApo :: Text -> Text
normApo = T.replace "’" "'"

-- |Sometimes the NLP tokenizer fails to split punctuation chars from words.
-- We fix that, but leave apostrophized suffixes such as "'ll" (split e.g. from
-- "I'll") alone.
splitPunct :: Text -> [Text]
splitPunct text | T.isPrefixOf "'" text = [text]
                | otherwise             = sepWordsFromNonWords text

pronounceTaggedLine :: PhoneticDict -> [Text] -> Tree.TaggedSentence Tag -> Text
pronounceTaggedLine pd ourTokens (Tree.TaggedSent posTags) =
    T.concat . reverse $ go [] tokensSplitAtSpace posTags
  where
    tokensSplitAtSpace :: [Text]
    tokensSplitAtSpace = concatMap (T.groupBy ((==) `on` isSpace)) ourTokens
    go :: [Text] -> [Text] -> [Tree.POS Tag] -> [Text]
    go acc (tok:tokens) (tag:tags)
      | T.strip (normApo tok) == Tree.showPOStok tag =
        go (pronounceTaggedToken pd (justPosTag tag) tok : acc) tokens tags
    go acc (tok:tokens) taglist | T.all isSpace tok =
        go (pronounceTaggedToken pd Nothing tok : acc) tokens taglist
    -- Contractions such as "I'll"
    go acc (tok:tokens) (t1:t2:tags) | normApo tok == joinTags t1 t2 =
        go (pronounceTaggedToken pd (justPosTag t1) tok : acc) tokens tags
    go acc [] [] = acc
    go _ tokens tags = error $ concat [
        "pronounceTaggedLine: Don't know how to unify <",
        T.unpack $ T.intercalate "/" tokens, "> with <",
        T.unpack . T.intercalate "/" . map Tree.showPOStok $ tags, ">"]
    joinTags :: Tree.POS Tag -> Tree.POS Tag -> Text
    joinTags t1 t2 = Tree.showPOStok t1 `T.append` Tree.showPOStok t2
    justPosTag :: Tree.POS Tag -> Maybe Text
    justPosTag = Just . Tree.showPOStag

-- |Convert a non-ambiguous token (word or non-word) into its pronounciation.
pronounceToken :: PhoneticDict -> Text -> Text
pronounceToken pd = pronounceTaggedToken pd Nothing

-- |Convert a token (word or non-word) into its pronounciation. The POS tag
-- (2nd argument) is used for ambiguation, if present and needed.
pronounceTaggedToken :: PhoneticDict -> Maybe Text -> Text -> Text
pronounceTaggedToken pd tag token | isLetter $ T.head token = adjustPron token
    $ pronounceWord pd tag (Map.lookup (CI.mk actualToken) pd) actualToken
  where
    actualToken | isGenitive token = T.dropEnd 2 token
                | otherwise        = token
pronounceTaggedToken _ _ token = token

isGenitive :: Text -> Bool
isGenitive = T.isSuffixOf "'s"

pronounceWord :: PhoneticDict -> Maybe Text -> Maybe PhoneticEntry -> Text
              -> Text
pronounceWord _ _ (Just (SimpleEntry pron)) _                = pron
pronounceWord pd _ (Just (RedirectEntry redirect)) _         =
    pronounceToken pd redirect
pronounceWord _ (Just tag) (Just (TaggedEntry tagMap)) token =
    pronounceTaggedEntry tag tagMap token
pronounceWord _ Nothing (Just (TaggedEntry _)) token =
     error $ concat ["pronounceWord: Don't know how to pronounce ",
                     "ambiguous word '", T.unpack token, "' without POS tag"]
-- Unknown lower-case words are prefixed with a question mark, while unknown
-- capitalized words as considered proper names and rendered as is.
pronounceWord _ _ Nothing token | isLower $ T.head token = '?' `T.cons` token
                                | otherwise              = token

pronounceTaggedEntry :: Text -> Map PosTag Text -> Text -> Text
pronounceTaggedEntry tag tagMap token
  | T.isPrefixOf "NN" tag, Just pron <- Map.lookup N tagMap     = pron
  | T.isPrefixOf "J" tag, Just pron <- Map.lookup Aj tagMap     = pron
  | T.isPrefixOf "VB" tag, Just pron <- Map.lookup V tagMap     = pron
  | T.isInfixOf "RB" tag, Just pron <- Map.lookup Av tagMap     = pron
  | tag `elem` ["CC", "IN"], Just pron <- Map.lookup Prp tagMap = pron
  | Just pron <- Map.lookup N tagMap                            = pron
  | Just pron <- Map.lookup Aj tagMap                           = pron
  | otherwise = error $ concat ["pronounceTaggedEntry: ",
        "Don't know how to pronounce ", T.unpack token, "/", T.unpack tag]

-- |Adjust a pronounciation by deleting its stress marker if it's redundant.
-- A stress marker is considered redundant if it's the only one and if it
-- occurs after the first non-schwa vowel in the word.
adjustPron :: Text -> Text -> Text
adjustPron token pron
  | isGenitive token = addGenitiveOrPlural deStressedPron
  | otherwise        = deStressedPron
  where
    deStressedPron
      | T.count "°" pron /= 1 = pron
      | otherwise             = if stressAfterFirstVowel
                                then T.replace "°" "" pron else pron
    stressAfterFirstVowel
      | T.length rest < 2                                  = False
      | T.isPrefixOf  "°" $ T.tail rest                    = True
      | T.isPrefixOf "aú°" rest || T.isPrefixOf "oi°" rest = True
      | otherwise                                          = False
    rest = T.dropWhile (not . isVowelButNotSchwa) pron

-- Add genitive or plural to the end of a pronounciation.
addGenitiveOrPlural :: Text -> Text
addGenitiveOrPlural pron = pron `T.append` genitiveOrPluralSound
  where
    genitiveOrPluralSound | isSibilant lastSound           = "'z"
                          | isVoicelessConsonant lastSound = "s"
                          | otherwise                      = "z"
    lastSound = findLastSound pron

-- Find the last sound in a pronounciation.
findLastSound :: Text -> Char
findLastSound "" = error "findLastSound: empty pronounciation"
findLastSound pron | T.last pron == '°' = findLastSound $ T.init pron
                   | otherwise          = T.last pron

isSibilant :: Char -> Bool
isSibilant ch = ch `elem` "jszćśź"

isVoicelessConsonant :: Char -> Bool
isVoicelessConsonant ch = ch `elem` "fkpstćśþ"
