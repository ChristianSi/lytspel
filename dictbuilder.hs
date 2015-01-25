-- Copyright (c) 2014-2015 Christian Siefkes
--
-- See accompanying LICENSE file for licensing information.
--
-- |Build a pronunciation dictionary for Phonetic English.

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Text.Read

import Control.Conditional (whenM)
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1, decodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.FilePath (combine)
import qualified System.Directory as Dir
import System.Process (readProcess)
import Text.EditDistance as ED

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
textToPos t = fromMaybe errorMsg result
  where
    result = readMaybe $ toUpper (T.head t) : T.unpack (T.tail t)
    errorMsg = error $
        "dictbuilder:textToPos: Not a valid POS tag: " ++ T.unpack t

posToText :: PosTag -> Text
posToText = T.toLower . T.pack . show

-- |A dictionary entry with an optional POS tag.
data DictEntry = DictEntry
    {deWord :: Text
    ,dePron :: Text
    ,dePos :: Maybe PosTag
    } deriving (Eq, Ord, Read, Show)

-- |Make a 'DictEntry' without a 'PosTag'.
untaggedDictEntry :: Text -> Text -> DictEntry
untaggedDictEntry word pron =
    DictEntry {deWord = word, dePron = pron, dePos = Nothing}

-- |Make a 'DictEntry' from a pronunciation, using an empty Text as word
-- and Nothing as POS tag.
pronToDictEntry :: Text -> DictEntry
pronToDictEntry pron =  DictEntry {deWord = "", dePron = pron, dePos = Nothing}

data Stress = MajorStress | WeakStress | NoStress
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

newtype Redirect = Redirect { rdTarget :: Text }

type CI2TextsMap    = Map (CI Text) [Text]
type CI2EntriesMap  = Map (CI Text) [DictEntry]
type CI2ExtendedMap = Map (CI Text) (Either [DictEntry] Redirect)
type TextMap        = Map Text Text

---- main and its helper functions ----

-- |Main entry point.
main :: IO ()
main = do
    wordMap <- buildWordMapFromScowl >>= addWordsNotInScowl
    cmudictProns <- collectCmudictPronunciations wordMap
    let unifiedCmudictProns = unifyWwithVoicelessW cmudictProns
    convertedCmudictProns <- convertCmudictPronunciations unifiedCmudictProns
    let cleanCmudictProns = Map.map ordNub $
                            cleanupCmudictInconsistencies convertedCmudictProns
    unifiedPronunciations <- addMobyPronunciations cleanCmudictProns wordMap
    completePronunciations <- addVarconRedirects unifiedPronunciations
    writePhoneticDict completePronunciations

-- |Build a mapping of words in SCOWL, omitting very rare words.
-- Each key is the lower-case form of a word, the corresponding value is the
-- most frequent cased form of the word.
buildWordMapFromScowl :: IO TextMap
buildWordMapFromScowl = do
    fileList <- listScowlFilesToRead
    listOfWordLists <- mapM readWordList fileList
    let decodedWordList = map decodeLatin1 $ concat listOfWordLists
        listWithoutGenitives = filter skipGenitives decodedWordList
        asciifiedList = map removeDiacritics listWithoutGenitives
    return $ wordListToMap Map.empty asciifiedList
  where
    readWordList :: String -> IO [B.ByteString]
    readWordList file  = strictByteLines <$> LB.readFile (fileInScowlDir file)
    fileInScowlDir     = combine "scowl/final"
    skipGenitives text = not $ "'s" `T.isSuffixOf` text

-- |Determine list of SCOWL files to read.
-- All files for American English up to frequency 80 are included, but for
-- proper names, only those up to frequency 50. Calls the `mk-list` utility
-- provided by SCOWL to generate the actual list. The result is sorted by
-- frequency (10 before 20 before 35 etc.).
listScowlFilesToRead :: IO [String]
listScowlFilesToRead = do
    cwd <- Dir.getCurrentDirectory
    Dir.setCurrentDirectory $ cwd `combine` "scowl"
    fileListing <- readProcess "mk-list" ["-f", "american", "80"] []
    Dir.setCurrentDirectory cwd
    let filteredListing = filter skipRareProperNames $ lines fileListing
    return $ sortBy (compare `on` numberInName) filteredListing
  where
    skipRareProperNames str = not $ "proper-names" `isInfixOf` str
                                  && numberInName str > 50
    numberInName str        = read (filter isDigit str) :: Int

-- |Break a lazy bytestring into a list of strict bytestrings at newline chars.
strictByteLines :: LB.ByteString -> [B.ByteString]
strictByteLines = map (B.concat . LB.toChunks) . LB.lines

-- |Replace the diacritical marks that occur in SCOWL and the Phonetic English
-- phoneme set by their ASCII equivalents, e.g. "é" by "e".
removeDiacritics :: Text -> Text
removeDiacritics t
  | T.all isAscii t = t
  | otherwise       = T.map replaceIfDiacritic t
  where
    replaceIfDiacritic ch | isAscii ch = ch
    replaceIfDiacritic 'Å' = 'A'
    replaceIfDiacritic 'ð' = 'd'
    replaceIfDiacritic 'ś' = 's'
    replaceIfDiacritic 'ẃ' = 'w'
    replaceIfDiacritic 'þ' = 't'
    replaceIfDiacritic 'ź' = 'z'
    replaceIfDiacritic ch
      | ch `elem` "àáâäå"  = 'a'
      | ch `elem` "çć"     = 'c'
      | ch `elem` "èéêë"   = 'e'
      | ch `elem` "íîï"    = 'i'
      | ch `elem` "ñń"     = 'n'
      | ch `elem` "óôöø"   = 'o'
      | ch `elem` "ùúûü"   = 'u'
      | otherwise          = error $ concat [
            "dictbuilder:removeDiacritics: Unexpected char '", [ch],
            "' in word: ", T.unpack t]

-- |Insert a list of texts into a map, mapping from a lower-case version of
-- each text to its original (possibly cased) form. If a lower-case version
-- of a text already exists in the map, the map is not changed.
wordListToMap :: TextMap -> [Text] -> TextMap
wordListToMap = foldl' addLowerMappingIfNew
  where
    addLowerMappingIfNew :: TextMap -> Text -> TextMap
    addLowerMappingIfNew m t = Map.insertWith (const id) (T.toLower t) t m

-- |Add relevant words that are missing in SCOWL.
addWordsNotInScowl :: TextMap -> IO TextMap
addWordsNotInScowl wordMap =
    wordListToMap wordMap . T.lines <$> T.readFile "words-not-in-scowl.txt"

-- |Build dictionary of pronunciations in cmudict.
-- Each key is a word as listed in SCOWL; each value is a list of
-- pronunciations (sometimes cmudict has multiple pronunciations for the same
-- word).
collectCmudictPronunciations :: TextMap -> IO CI2TextsMap
collectCmudictPronunciations wordMap = do
    contents <- LB.readFile "cmudict/cmudict.0.7a"
    let entries = map decodeUtf8 $ strictByteLines contents
        regularEntries = map T.toLower $ filter isRegularEntry entries
        dictEntries = map (stripFinalParens . splitAtFirstWS) regularEntries
        justEntriesInScowl = mapMaybe (entryAsInScowl wordMap) dictEntries
    return $ foldr addCmudictEntry Map.empty justEntriesInScowl
  where
    isRegularEntry line    = isAsciiUpper $ T.head line
    stripFinalParens :: (Text, Text) -> (Text, Text)
    stripFinalParens (word, pron)
      | T.last word == ')' = (T.takeWhile (/= '(') word, pron)
      | otherwise          = (word, pron)
    addCmudictEntry :: (Text, Text) -> CI2TextsMap -> CI2TextsMap
    addCmudictEntry (word, pron) = Map.insertWith (++) (CI.mk word) [pron]

-- |Split a text at the first whitespace sequence.
-- Any whitespace at the end of the text is stripped.
splitAtFirstWS :: Text -> (Text, Text)
splitAtFirstWS text = (word, T.strip rest)
  where (word, rest) = T.break isSpace text

-- |Look up '(word, pronunciation)' tuple in the SCOWL word map. 
-- If the word exists there, the result is 'Just (wordInScowl, pronunciation)',
-- where wordInScowl is the word in the capitalization as used in SCOWL.
entryAsInScowl :: TextMap -> (Text, Text) -> Maybe (Text, Text)
entryAsInScowl wordMap (word, pron) = case Map.lookup word wordMap of
    Just wordInScowl -> Just (wordInScowl, pron)
    Nothing          -> Nothing

-- |Unify "w" and "hh w" [ẃ] where cmudict has both as alternatives.
-- We unify this to "hw" for further processing.
unifyWwithVoicelessW :: CI2TextsMap -> CI2TextsMap
unifyWwithVoicelessW = Map.map unify
  where
    unify, unified, replaced, obsolete :: [Text] -> [Text]
    unify prons@(_:_:_) | any hhwIsInfix prons = unified prons
    unify prons                                = prons
    unified prons  = filter (`notElem` obsolete prons) $ replaced prons
    replaced       = map $ T.replace "hh w" "hw"
    obsolete prons = map (T.replace "hh w" "w") $ filter hhwIsInfix prons
    hhwIsInfix     = T.isInfixOf "hh w"

-- |Convert cmudict pronunciations to the Phonetic English phoneme set.
convertCmudictPronunciations  :: CI2TextsMap -> IO CI2TextsMap
convertCmudictPronunciations cmudictProns = do
    phonemeMap <- readKeyValueFile "cmudict-phonemes.txt"
    return $ Map.map (convertCmudictProns phonemeMap) cmudictProns
  where
    convertCmudictProns :: TextMap -> [Text] ->  [Text]
    convertCmudictProns pm = map $ convertCmudictPron pm

-- |Read a key-value file. Keys and values are separated by ':'; trailing
-- comments introduced by '#' are stripped.
readKeyValueFile :: FilePath -> IO TextMap
readKeyValueFile file =
    liftM (foldl' insertKeyValuePair Map.empty . T.lines) $ T.readFile file

-- |Insert a line from a key-value file into a map.
insertKeyValuePair :: TextMap -> Text -> TextMap
insertKeyValuePair m line = Map.insert (T.strip rawKey) value m
  where
    value              = T.strip . T.takeWhile (/= '#') . T.tail $ rawValue
    (rawKey, rawValue) = T.break (== ':') line

convertCmudictPron :: TextMap -> Text -> Text
convertCmudictPron phonemeMap pron = stressFirstVowelIfNoStress
    . moveStressMarkBeforeR . T.concat $ map convertSound sounds
  where
    sounds :: [Text]
    sounds = T.words pron
    convertSound s
      | Just conv <- lookupPhoneme s = conv
      | Just conv <- lookupPhoneme $ T.init s, T.last s == majorStressMarker =
            conv `T.snoc` '°'
      | Just conv <- lookupPhoneme $ T.init s, isStressHint $ T.last s = conv
      | otherwise          = error $ concat [
            "dictbuilder:convertCmudictPron: Unknown phoneme '", T.unpack s,
            "' in pronunciation: ", T.unpack pron]
    lookupPhoneme :: Text -> Maybe Text
    lookupPhoneme s       = Map.lookup s phonemeMap
    majorStressMarker     = if T.any (== '1') pron then '1' else '2'
    isStressHint ch       = ch `elem` "012"
    moveStressMarkBeforeR = T.replace "r°" "°r"

-- |Mark the first vowel as stressed if none is. Also fixes stress markers
-- in the diphthongs [aú] and [oi].
stressFirstVowelIfNoStress :: Text -> Text
stressFirstVowelIfNoStress pron = fixDiphthongStress pronWithStress
  where
    pronWithStress
      | not (T.any (== '°') pron) && T.any isVowelButNotSchwa pron =
            T.concat [beforeFirstVowel, T.take 1 rest, "°", T.tail rest]
      | otherwise                                 = pron
    (beforeFirstVowel, rest) = T.break isVowelButNotSchwa pron
    fixDiphthongStress = T.replace "a°ú" "aú°" . T.replace "o°i" "oi°"

isVowelButNotSchwa :: Char -> Bool
isVowelButNotSchwa ch = ch `elem` "aeiouäëïöüáéóú"

isVowel :: Char -> Bool
isVowel '\'' = True
isVowel ch   = isVowelButNotSchwa ch

-- |Remove some ambiguities and inconsistencies in the cmudict data.
cleanupCmudictInconsistencies :: CI2TextsMap -> CI2TextsMap
cleanupCmudictInconsistencies cmudictProns =
    Map.mapWithKey cleanupProns cmudictProns
   where
    cleanupProns :: CI Text -> [Text] -> [Text]
    cleanupProns word = map $ cleanupInconsistencies cmudictProns word

cleanupInconsistencies :: CI2TextsMap -> CI Text -> Text -> Text
cleanupInconsistencies cmudictProns word pron
  -- "trans" should be [trans] rather than [tranz], except before vowel
  | "trans" `T.isInfixOf` CI.foldedCase word,
    Just (prefix, match, suffix) <- breakOnOneOf ["tranz", "tra°nz"] pron,
    doesntStartWithVowel suffix =
        T.concat [prefix, T.init match, "s", suffix]
  -- Unstressed "semi-" in compounds should be [semi]
  | "semi" `T.isPrefixOf` CI.original word,
    ciDrop 4 word `Map.member` cmudictProns,
    not $ T.any (`elem` "°'") $ T.take 4 pron =
        "semi" `T.append` T.drop 4 pron
  -- Initial ['] should be [u] if word starts with "u"
  | T.head pron == '\'', T.head (CI.foldedCase word) == 'u' =
        'u' `T.cons` T.tail pron
  -- Initial "au" should be [ó] rather than one of [oá']
  | "au" `T.isPrefixOf` CI.foldedCase word, T.head pron `elem` "oá'" =
        'ó' `T.cons` T.tail pron
  -- Initial "ar" should be [ár]  rather than [or]
  | "ar" `T.isPrefixOf` CI.foldedCase word, T.head pron == 'o' =
        'á' `T.cons` T.tail pron
  -- Initial vowel should be [ä] if word starts with "air" or "aer"
  | "air" `T.isPrefixOf` CI.foldedCase word
    || "aer" `T.isPrefixOf` CI.foldedCase word = 'ä' `T.cons` T.tail pron
  -- Initial [u] should otherwise be [a] if word starts with "a"
  | T.head pron == 'u', T.head (CI.foldedCase word) == 'a' =
        'a' `T.cons` T.tail pron
    -- Initial [á] should be [o] if word starts with "o" (except before "r")
  | T.head pron == 'á', T.head (CI.foldedCase word) == 'o',
    not $ "or" `T.isPrefixOf` CI.foldedCase word = 'o' `T.cons` T.tail pron
  -- German "kind" is spoken with short [i]
  | CI.foldedCase word == "wunderkind" = T.replace "ï" "i" pron
  | otherwise                               = pron
  where
    doesntStartWithVowel t
      | T.null t    = True
      | otherwise   = not . isVowel $ T.head t

-- |Find the first instance of one of the needles in a haystack.
-- Yields 'Just (prefix, match, suffix)' in case of a match, 'Nothing'
-- otherwise. If several of the needles occur in the haystack, the first one
-- in the list wins, not the first in the text.
breakOnOneOf :: [Text] -> Text -> Maybe (Text, Text, Text)
breakOnOneOf [] _ = Nothing
breakOnOneOf (t:ts) haystack
  | T.null rest = breakOnOneOf ts haystack
  | otherwise   = Just (prefix, t, T.drop (T.length t) rest)
  where (prefix, rest) = T.breakOn t haystack

-- |Drop n characters from a CI Text.
ciDrop :: Int -> CI Text -> CI Text
ciDrop n = CI.mk . T.drop n . CI.original

-- |Remove duplicate elements from a list, keeping the first occurrence of
-- each element. Source: https://github.com/nh2/haskell-ordnub.
ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go :: (Ord a) => Set a -> [a] -> [a]
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x:go (Set.insert x s) xs

addMobyPronunciations :: CI2TextsMap -> TextMap -> IO CI2EntriesMap
addMobyPronunciations cmudictProns wordMap = do
    contents <- LB.readFile "moby/mpron.txt"
    let entries = map decodeLatin1 $ strictByteLines contents
        relevantEntries = foldr prepareEntry Map.empty entries
        unifiedEntries = Map.mapMaybeWithKey unifyMobyEntries relevantEntries
    phonemeMap <- readKeyValueFile "moby-phonemes.txt"
    let mobyDict   = Map.mapMaybe (convertMobyProns cmudictProns phonemeMap)
                     unifiedEntries
        unifiedMap = Map.unionWithKey unifyMobyWithCmudict mobyDict cmuDict
    return $ Map.mapWithKey keepJustOneUntaggedEntry unifiedMap
  where
    prepareEntry :: Text -> CI2EntriesMap -> CI2EntriesMap
    prepareEntry = addEntryIfRelevant wordMap . makeDictEntry . splitAtFirstWS
    cmuDict = Map.map (map pronToDictEntry) cmudictProns

-- |If there are multiple untagged entries (from cmudict), we keep the most
-- similar one.
keepJustOneUntaggedEntry :: CI Text -> [DictEntry] -> [DictEntry]
keepJustOneUntaggedEntry _ [entry]    = [entry]
keepJustOneUntaggedEntry _ entries | isJust $ dePos $ head entries = entries
keepJustOneUntaggedEntry word entries = [mostSimilarEntry word entries]

mostSimilarEntry :: CI Text -> [DictEntry] -> DictEntry
mostSimilarEntry word = minimumBy (compare `on` editDistanceFromWord)
  where
    editDistanceFromWord de = ED.levenshteinDistance ED.defaultEditCosts
                              (preparePron de) (T.unpack $ CI.foldedCase word)
    preparePron = T.unpack . removeDiacritics . T.replace "°" "" . dePron

-- Create a DictEntry from a Moby entry already split at first whitespace.
-- Handles POS tags such as "/n".
makeDictEntry :: (Text, Text) -> DictEntry
makeDictEntry (first, pron)
  | T.any (== '/') first = DictEntry
        {deWord = T.init wordWithTrailingSlash
        ,dePron = pron
        ,dePos = Just $ textToPos rawPos
        }
  | otherwise            = untaggedDictEntry first pron
  where
    (wordWithTrailingSlash, rawPos) = T.breakOnEnd "/" first

-- |Add a DictEntry if the word is either listed in SCOWL or starts with a
-- common prefix and the rest is in SCOWL.
addEntryIfRelevant :: TextMap -> DictEntry -> CI2EntriesMap -> CI2EntriesMap
addEntryIfRelevant wordMap de m
  | Just wordInScowl <- Map.lookup lower wordMap =
        Map.insertWith (++) (CI.mk wordInScowl) [de] m
  | T.any (== '-') lower,
    [prefix, suffix] <- T.splitOn "-" lower,
    prefix `Map.member` commonPrefixMap,
    Just suffixInScowl <- Map.lookup suffix wordMap = Map.insertWith (++)
        (CI.mk suffixInScowl) [strippedEntry prefix suffix] m
  | otherwise           = m
  where
    lower = T.toLower $ deWord de
    strippedEntry :: Text -> Text -> DictEntry
    strippedEntry pf sf = forceStripPronPrefix (commonPrefixMap ! pf) sf de

-- |A mapping from common prefixes to how they are pronounced in Moby.
commonPrefixMap :: Map Text [Text]
commonPrefixMap = Map.fromList
    [ ("cross", ["kr/O/s", "kr/A/s", "kr/A//S/"])
    , ("ill", ["/I/l"])
    , ("neo", ["n/i//oU", "n/E//oU"])
    , ("non", ["n/@/n", "n/A/n", "n/A//N/"])
    , ("post", ["p/oU/st"])
    , ("self", ["s/E/lf"])
    , ("uto", ["/j//u/t/oU"])
    ]

-- |Strip a prefix from the pronunciation of a DictEntry.
-- If the pronunciation doesn't start with one of the prefixes given as first
-- argument, an error is thrown. There may be a Moby stress marker
-- ("'" or ",") in front of the prefix, which will be stripped as well.
-- If "_" or "/_" follows after the prefix, it will also be stripped.
-- 'deWord' will be set to 'wordSuffix' preceded by a hyphen (as a reminder
-- that the entries comes from a compound).
forceStripPronPrefix :: [Text] -> Text -> DictEntry -> DictEntry
forceStripPronPrefix prefixes wordSuffix de =
    de {deWord = '-' `T.cons` wordSuffix, dePron = cleanedPron}
  where
    cleanedPron  = stripOnePrefix ["_", "/_"] strippedPron
    strippedPron = forceStripOnePrefix prefixes pronWithoutInitialStress
    pronWithoutInitialStress
      | T.head (dePron de) `elem` "'," = T.tail $ dePron de
      | otherwise                      = dePron de
    forceStripOnePrefix :: [Text] -> Text -> Text
    forceStripOnePrefix ps t
      | t' <- stripOnePrefix ps t, t' /= t = t'
      | otherwise = error $ concat ["dictbuilder:forceStripPronPrefix: ",
          "Couldn't force-strip prefix from ", T.unpack (dePron de)]

-- |Strip one of the prefixes listed as first argument from a text.
-- If the text doesn't start with any of them, it is returned unchanged.
stripOnePrefix :: [Text] -> Text -> Text
stripOnePrefix [] t = t
stripOnePrefix (p:ps) t
  | Just rest <- T.stripPrefix p t = rest
  | otherwise                      = stripOnePrefix ps t

-- |Unify entries for a word from Moby, discarding redundant entries.
unifyMobyEntries :: CI Text -> [DictEntry] -> Maybe [DictEntry]
-- If there is just one entry, we simply return it.
unifyMobyEntries _ [de] = Just [de]
unifyMobyEntries word des
  -- If there are 2 or more POS-tagged entries, we generally just keep them
  -- (but "frequent/n" is unnecessary and discarded)
  | length taggedEntries > 1 = Just $ if CI.original word == "frequent"
        then [head taggedEntries, last taggedEntries] else taggedEntries
  -- There are just 3 words with a single tagged entry, and we deal with
  -- each of them as appropriate
  | length taggedEntries == 1 = case CI.original word of
      "predicate"  -> Just $ tagAs N (head untaggedEntries) : taggedEntries
      "ingenerate" -> Just $ taggedEntries ++ [tagAs V $ head untaggedEntries]
      "ingeminate" -> Just untaggedEntries
      _            -> throwError
  -- If there are only untagged entries, we keep the one that's spelled as in
  -- SCOWL (others use different capitalization or come from compounds)
  | length filteredUntaggedEntries == 1 = Just filteredUntaggedEntries
  -- If none of the entries is spelled as in SCOWL, we discard the entry
  -- altogether if it's all-caps (e.g. LA, MIDI, WAF)
  | null filteredUntaggedEntries = if T.all isAsciiUpper (CI.original word)
        then Nothing
        -- Otherwise we keep the first entry that's not from a compound
        else Just [head $ filter (('-' /=) . T.head . deWord) des]
  -- Otherwise something went wrong
  | otherwise                    = throwError
  where
    (taggedEntries, untaggedEntries) = partition (isJust . dePos) des
    tagAs :: PosTag -> DictEntry -> DictEntry
    tagAs pos entry = entry {dePos = Just pos}
    throwError= error $ concat [ "dictbuilder:unifyMobyEntries: ",
        "Don't know how to unify entries for ", T.unpack (CI.original word)]
    filteredUntaggedEntries   = filter keepIfWordMatchesScowl untaggedEntries
    keepIfWordMatchesScowl de = deWord de == CI.original word

convertMobyProns :: CI2TextsMap -> TextMap -> [DictEntry] -> Maybe [DictEntry]
convertMobyProns cmudictProns phonemeMap des = if null convertedEntries
        then Nothing else Just convertedEntries
  where
    convertedEntries :: [DictEntry]
    convertedEntries = mapMaybe (convertMobyEntry cmudictProns phonemeMap) des

-- |Convert the pronunciation of a Moby entry. If the pronunciation contains
-- a transcription mistake that cannot be fixed unambiguously, 'Nothing' is
-- returned instead.
convertMobyEntry :: CI2TextsMap -> TextMap -> DictEntry -> Maybe DictEntry
convertMobyEntry cmudictProns phonemeMap de =
   if any (`elem` ["c", "e", "o"]) sounds then Nothing
                                          else Just $ de {dePron = fixedPron}
  where
    sounds :: [Text]
    sounds = concatMap splitIfUnknown . concatMap splitAtStressMarkers .
             filter (not . T.null) . T.split (== '/') . fixMistakes $ dePron de
    -- Replace "sh" by "S" (occasional transcription mistake)
    fixMistakes = T.replace "sh" "S"
    splitAtStressMarkers :: Text -> [Text]
    splitAtStressMarkers s
      | isShortOrPhoneme s = [s]
      | otherwise          = if T.any isStressMarker $ replaced s
            then T.groupBy ((==) `on` isStressMarker) $ replaced s
            else [replaced s]
    replaced = T.map replaceSpecials
    -- A and N denote different sounds when they aren't surrounded by "/"
    replaceSpecials 'A' = 'à'
    replaceSpecials 'N' = 'ǹ'
    replaceSpecials ch  = ch
    isShortOrPhoneme s  = T.length s == 1 || s `Map.member` phonemeMap
    isStressMarker ch   = ch `elem` "',"
    splitIfUnknown :: Text -> [Text]
    splitIfUnknown s | isShortOrPhoneme s = [s]
                     | otherwise          = T.chunksOf 1 s
    fixedPron         = cleanupMobyInconsistencies cmudictProns
                        (CI.mk $ deWord de) convertedPron
    convertedPron     = convertMobySounds phonemeMap majorStressMarker sounds
    majorStressMarker = if T.any (== '\'') (dePron de) then "'" else ","


-- |Remove some ambiguities and inconsistencies in the Moby data.
cleanupMobyInconsistencies :: CI2TextsMap -> CI Text -> Text -> Text
cleanupMobyInconsistencies cmudictProns word pron
  -- Moby doesn't have [ó] so we try to fix that in some cases
  | wordContainsOrNotFollowedByROrVowel, "or" `T.isInfixOf` pron'  =
        T.replace "or" "ór" pron'
  | wordContainsOrNotFollowedByROrVowel, "o°r" `T.isInfixOf` pron' =
        T.replace "o°r" "ó°r" pron'
  | "aul" `T.isInfixOf` CI.foldedCase word, "ol" `T.isInfixOf` pron'    =
        T.replace "ol" "ól" pron'
  | "aul" `T.isInfixOf` CI.foldedCase word, "o°l" `T.isInfixOf` pron'   =
        T.replace "o°l" "ó°l" pron'
  | otherwise = pron'
  where
    pron' = cleanupMobyVowels word . cleanupGeneralAndConsonantIssues
            cmudictProns word $ pron
    wordContainsOrNotFollowedByROrVowel =
        length wordParts > 1 && isEmptyOrStartsWithROrVowel (wordParts !! 1)
    wordParts :: [Text]
    wordParts = T.splitOn "or" $ CI.foldedCase word
    isEmptyOrStartsWithROrVowel "" = True
    isEmptyOrStartsWithROrVowel t  =
        not $ T.head t == 'r' || isVowel (T.head t)

cleanupMobyVowels :: CI Text -> Text -> Text
cleanupMobyVowels  word =
    -- Sometimes [á] is used instead of [o]
    replaceWithIfWordContainsButNot "dá" "do" "do" ["dah", "dan"] word
    . replaceWithIfWordContainsButNot "ká" "ko" "co" ["ca"] word
    . replaceWithIfWordContainsButNot "lá" "lo" "lo" ["lala", "lalo"] word
    . replaceWithIfWordContainsButNot "má" "mo" "mo" ["mam", "marm"] word
    . replaceWithIfWordContainsButNot "rá" "ro" "ro" ["grad", "rab", "rag"] word
    . replaceWithIfWordContainsButNot "tá" "to" "to" ["ta"] word
    . replaceWithIfWordContainsButNot "ná" "no" "no" [] word
    -- Sometimes [o] is used instead of [á]
    . replaceWithIfWordContainsButNot "o°l" "á°l" "al" [] word
    . replaceWithIfWordContainsButNot "ol" "ál" "al" ["ohol"] word
    -- Sometimes [u] is used instead of [o]
    . replaceWithIfWordContainsButNot "ku°n" "ko°n" "con" [] word
    -- [u°r] should be [é°r] if written "er"
    . replaceWithIfWordContainsButNot "u°r" "é°r" "er"
      ["erg", "erpe", "err", "ers", "erva"] word
    -- "gate" should be [gät] instead of [gu°t]
    . replaceWithIfWordContainsButNot "gu°t" "gä°t" "gate" [] word

-- |Call 'cleanupInconsistencies' and replace [ll] by [l] where appropriate.
cleanupGeneralAndConsonantIssues :: CI2TextsMap -> CI Text -> Text -> Text
cleanupGeneralAndConsonantIssues cmudictProns word pron
  | "ll" `T.isInfixOf` pron', not $ "lless" `T.isSuffixOf` CI.foldedCase word
    || "leless" `T.isSuffixOf` CI.foldedCase word = T.replace "ll" "l" pron'
  | otherwise = pron'
  where pron' = cleanupInconsistencies cmudictProns word pron

-- |Replace a fragment of the pronunciation if the written word contains one
-- fragment but not others.
replaceWithIfWordContainsButNot :: Text -> Text -> Text -> [Text] -> CI Text
                                -> Text -> Text
replaceWithIfWordContainsButNot original replacement inWord noneInWord word pron
  | inWord `T.isInfixOf` CI.foldedCase word,
    not $ any (`T.isInfixOf` CI.foldedCase word) noneInWord =
        T.replace original replacement pron
  | otherwise                                               = pron

-- |Convert a Moby pronunciation that has already been split into a sequence of
-- phonemes and stress markers.
convertMobySounds :: TextMap -> Text -> [Text] -> Text
convertMobySounds phonemeMap majorStressMarker = stressFirstVowelIfNoStress
      . T.concat . snd . mapAccumL convertSound NoStress
  where
    convertSound :: Stress -> Text -> (Stress, Text)
    convertSound stress sound
      | Just conv <- lookupPhoneme sound = handleStress stress conv
      | sound == majorStressMarker       = (MajorStress, "")
      | sound == ","                     = (WeakStress, "")
      | ignorable sound                  = (stress, "")
      | otherwise = error $
            "dictbuilder:convertMobySounds: Unknown phoneme: " ++ T.unpack sound
    lookupPhoneme :: Text -> Maybe Text
    lookupPhoneme s = Map.lookup s phonemeMap
    handleStress :: Stress -> Text -> (Stress, Text)
    handleStress MajorStress sound
      | T.any isVowelButNotSchwa sound = (NoStress, sound `T.snoc` '°')
    handleStress WeakStress sound
      | T.any isVowelButNotSchwa sound = (NoStress, sound)
    -- unstressed [@] should be ['] rather than [u]
    handleStress NoStress "u"          = (NoStress, "'")
    handleStress stress sound          = (stress, sound)
    ignorable s = T.length s == 1 && T.head s `elem` "_ ()"

unifyMobyWithCmudict :: CI Text -> [DictEntry] -> [DictEntry] -> [DictEntry]
unifyMobyWithCmudict word mobyEntries cmudictEntries
  -- If Moby has just one entry which is also in cmudict, we keep it
  | mobyEntryCount == 1 = if cmudictHasMatchingEntry firstMobyEntry
        then [firstMobyEntry]
        -- Keep the entry that's most similar to the word by edit distance
        else [mostSimilarEntry word (cmudictEntries ++ mobyEntries)]
  -- If there are two or more (POS-tagged) Moby entries, we keep them
  | mobyEntryCount > 1 = mobyEntries
  | otherwise = error $ concat ["dictbuilder:unifyMobyWithCmudict: ",
      "Don't know how to unify entries for: ", T.unpack $ CI.original word]
  where
    mobyEntryCount = length mobyEntries
    firstMobyEntry = head mobyEntries
    cmudictHasMatchingEntry de = any ((== dePron de) . dePron) cmudictEntries

-- |Add redirects for variant spellings, e.g. from "colour" to "color".
addVarconRedirects :: CI2EntriesMap -> IO CI2ExtendedMap
addVarconRedirects dict = do
      ls <- strictByteLines <$> LB.readFile "varcon/varcon.txt"
      let relevantLines = map decodeLatin1 $ filter skipEmptyAndCommentLines ls
          wordLists     = filter skipTrivialEntries . map lineToWords
                          $ relevantLines
      return $ foldl' addRedirects convertedMap wordLists
  where
    skipEmptyAndCommentLines line = not $ B.null line || B.head line == '#'
    lineToWords :: Text -> [Text]
    lineToWords line = mapMaybe (discardTagInfoAndVariants . discardNotes)
                       $ T.splitOn "/" line
    -- Discard anything after "|" or "#"
    discardNotes = head . T.split (`elem` "|#")
    -- Skip entries that are just genitive forms (ending in "'s") or don't
    -- contain at least 2 variants
    skipTrivialEntries :: [Text] -> Bool
    skipTrivialEntries texts
      | length texts < 2 = False
      | otherwise = not $ "'s" `T.isSuffixOf` head texts
    convertedMap = Map.foldrWithKey convertToLeft Map.empty dict
    convertToLeft :: CI Text -> [DictEntry] -> CI2ExtendedMap
                  -> CI2ExtendedMap
    convertToLeft word des = Map.insert word $ Left des

-- |Discard tagging info such as "A Av1 B Bv". If all tags are merely
-- variants (containing a marker such as "v"), the whole word is discarded
-- and 'Nothing' is returned.
discardTagInfoAndVariants :: Text -> Maybe Text
discardTagInfoAndVariants text = if all (T.any (`elem` "v.-x")) tags
                                    then Nothing else Just $ T.strip word
  where
    word = last splitted
    tags, splitted :: [Text]
    tags     = T.words . T.toLower . head $ splitted
    splitted = T.splitOn ":" text

-- |Add redirects from unlisted words to the first listed word (if any).
addRedirects :: CI2ExtendedMap -> [Text] -> CI2ExtendedMap
addRedirects m texts
  | not $ null listedWords || null unlistedWords =
    foldl' insertRedirects m unlistedWords
  | otherwise = m
  where
    (listedWords, unlistedWords) = partition inDict texts
    inDict                       = flip Map.member m . CI.mk
    insertRedirects :: CI2ExtendedMap -> Text -> CI2ExtendedMap
    insertRedirects m' text = Map.insert (CI.mk text)
                                  (Right . Redirect . head $ listedWords) m'

-- |Write the phonetic dictionary into a line file.
-- If there is just a single pronunciations, the entry is written as
-- 'word: pron'. If the pronunciation of a word depends on which POS
-- (part-of-speed) it is, it is written as 'word/n: pron1; v: pron2'
-- (where "n", "v" etc. are POS tags).
-- Redirects are written as 'word:> target', e.g. 'colour:> color'.
writePhoneticDict :: CI2ExtendedMap -> IO ()
writePhoneticDict m = do
    renameToBackupFileIfExists file
    LT.writeFile file contents
  where
    file                 = "phonetic-dict.txt"
    contents             = lazyUnlines $ Map.foldrWithKey addLine [] m
    addLine :: CI Text -> Either [DictEntry] Redirect -> [Text] -> [Text]
    addLine word (Left des) = addLeft word des
    addLine word (Right rd) = addRight word rd
    addLeft :: CI Text -> [DictEntry] -> [Text] -> [Text]
    addLeft word [de] ls = T.concat [CI.original word, ": ", showEntry de] : ls
    addLeft word des ls  = T.concat [CI.original word, "/",
                               T.intercalate "; " $ map showEntry des] : ls
    showEntry de = showPos (dePos de) `T.append` dePron de
    showPos :: Maybe PosTag -> Text
    showPos Nothing    = ""
    showPos (Just pos) = posToText pos `T.append` ": "
    addRight :: CI Text -> Redirect -> [Text] -> [Text]
    addRight word rd ls = T.concat [CI.original word, ":> ", rdTarget rd] : ls

-- |Rename a file into a backup file by appending ".bak" to its name.
-- A previously existing backup file with the same name will be silently
-- overwritten. If 'file' doesn't exist, this function does nothing.
renameToBackupFileIfExists :: FilePath -> IO ()
renameToBackupFileIfExists file =
    whenM (Dir.doesFileExist file) $ Dir.renameFile file $ file ++ ".bak"

-- |Join multiple strict texts into a long lazy text, after appending a
-- terminating newline to each of them.
lazyUnlines :: [Text] -> LT.Text
lazyUnlines texts = LT.fromChunks $ intersperse "\n" texts ++ ["\n"]
