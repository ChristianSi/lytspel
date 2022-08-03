"""Defines the dictionary mapping tradspell to Lytspel."""
# pylint: disable=consider-using-f-string, no-else-return

import csv
from enum import Enum
from functools import lru_cache
from io import TextIOWrapper
import re
from typing import Optional, List, Tuple, Union
from warnings import warn

from pkg_resources import resource_stream

from .util import get_elem


# Non-ASCII letters which we know how to convert to ASCII (lower-case only).
HANDLED_NON_ASCII_LETTERS = 'áàăâǎåäãąāảæćĉčċçďđéèĕêěëẽęēẻǵğĝǧġḡĥȟḧħíìĭîǐïĩįīĵǰḱǩĺľłḹḿńǹňñ'\
                            'óòŏôǒöõøǫōỏœṕŕřśŝšşßťẗúùŭûǔůüũųūủṽẃẁẘẅẍýỳŷẙÿỹȳỷźẑž'

# Matches a non-ASCII letter which we know how to convert.
# Note: precompiling all regexes, while not as readable, actually speeds up the conversion
# process quite a bit.
NON_ASCII_LETTERS_RE = re.compile('[{}]'.format(HANDLED_NON_ASCII_LETTERS), re.IGNORECASE)

# Matches a capitalized letter followed by a lower-case one (optionally with something else
# between them).
CAPTALIZED_RE = re.compile('[A-Z].*[a-z]')

# Matches a capitalized letter occurring after something else.
INNER_CAP_RE = re.compile('.[A-Z]')

# Matches all-caps words.
ALL_CAPS_RE = re.compile("[-'A-Z]+$")

# Matches an upper-case letter.
UPPER_CASE_RE = re.compile('[A-Z]')

# Matches a single-letter contraction such as "'d".
SINGLE_LETTER_CONTRACTION_RE = re.compile("'.$")

# Matches 'I' and its contractions.
I_AND_CONTRACTIONS_RE = re.compile("I$|I['’]")

# Matches words ending in "'s" (genitive) or generic contractions such as "'ve".
GENERIC_CONTRACTION_RE = re.compile(".['’](s|d|ll|re|ve)$", re.IGNORECASE)

# All-caps abbreviations that should be kept unchanged
CAP_ABBREVS = frozenset(('BIOS', 'DE', 'ETA', 'GA', 'HI', 'LA', 'MA', 'ME', 'MI', 'OH', 'PA', 'US',
                         'WA'))

POS_FALLBACKS = {'aj': 'n', 'av': 'aj', 'n': 'aj', 'prp': 'aj', 'v': 'n'}

ConvState = Enum('ConvState', 'LOOKS_FOREIGN NLP_NEEDED')  # pylint: disable=invalid-name


class Dictionary:
    """A dictionary mapping tradspell to Lytspel.

    Instances of this class are threadsafe after initialization. Usually it should be
    enough to have a single instance per program.
    """

    def __init__(self):
        """Initializes and loads the dictionary."""
        ##printmsg('Loading the dictionary.')
        self._dict = {}
        self._mixed_dict = {}

        with TextIOWrapper(resource_stream('lytspel', 'lytspel-dict.csv'),
                           encoding='utf-8') as csv_stream:
            csvreader = csv.reader(csv_stream)
            next(csvreader)  # skip header line
            redirects = {}  # will be resolved later

            for row in csvreader:
                tradspell = get_elem(row, 0)
                pos = get_elem(row, 1)
                redirect = get_elem(row, 2)
                lytspel = get_elem(row, 3)

                if tradspell and lytspel:
                    ts_lower = tradspell.lower()
                    ls_lower = lytspel.lower()

                    if pos:
                        # Treat value as a dict of POS-tagged entries
                        if not ts_lower in self._dict:
                            self._dict[ts_lower] = {}
                        self._dict[ts_lower][pos] = ls_lower
                    else:
                        self._dict[ts_lower] = ls_lower

                    if self.is_mixed_case(lytspel):
                        self._mixed_dict[tradspell] = lytspel

                elif tradspell and redirect:
                    redirects[tradspell.lower()] = redirect.lower()
                else:
                    warn('Unexpected/malformed CSV row: {}'.format(','.join(row)))

        # Resolve redirects
        for key, target in redirects.items():
            value = self._dict.get(target)
            if value:
                self._dict[key] = value
            else:
                warn("Target '{}' of redirect '{}' missing!".format(target, key))

    @staticmethod
    def is_mixed_case(word: str) -> bool:
        """Test whether a word is MiXed case.

        A word is assumed to be MiXed case if it contains at least one upper-case letter
        that's not the first letter and at least one lower-case letter. Words must be
        ASCII-fied for this function to work correctly.
        """
        return bool(CAPTALIZED_RE.search(word)) and bool(INNER_CAP_RE.search(word))

    @lru_cache(maxsize=1048576)
    def lookup(self, word: str, spacy_pos: str = None, at_sent_start: bool = False) -> Union[
            str, ConvState]:
        """Lookup a word in the dictionary and returned its converted form.

        'spacy_pos' is the POS tag as returned by spaCy. If it's omitted and if the respelling
        of the word depends on its POS tag, ConvState.NLP_NEEDED is returned.

        'at_sent_start' should be set to true if this token is the first of a new sentence.
        It is used to handle the case of the converted pronoun 'I' and its contractions.

        None is returned if the word is unknown.

        Case is restored (lower, Capitalized, or ALL_CAPS). MixedCase is also restored
        provided that *both* the input word and the dictionary entry use this case form
        (e.g. JavaScript -> JaavaScript).
        """
        # pylint: disable=too-many-branches
        word = self.asciify(word)
        # Strip final "'s" (genitive) or contraction and remember for later (handling case)
        word, contraction = self.strip_generic_contraction(word)

        if word in CAP_ABBREVS:
            result = word
        else:
            lower = word.lower()

            if lower in ("'ll", "'re", "'ve"):
                # Contraction tokens produced by spaCy: discard the last letter
                result = lower[:-1]
            elif SINGLE_LETTER_CONTRACTION_RE.match(lower):
                # Single-letter contraction token: return as is
                result = lower
            else:
                result = self._dict.get(lower)

                if result is None:
                    result = self.fallback_lookup(lower)
                    if result is None:
                        # Still no match
                        return None

                if isinstance(result, dict):
                    if spacy_pos:
                        result = self._find_pos_tagged_entry(result, spacy_pos)

                    else:
                        return ConvState.NLP_NEEDED

            result = self._restore_capitalization(result, word)

            if not at_sent_start and I_AND_CONTRACTIONS_RE.match(word) and result != 'I':
                # Correct the case of the pronoun 'I' and its contractions
                result = result.lower()

        if contraction:
            if len(contraction) == 3:
                # Strip the final letter of two-letter contractions such as "'ll" or "'ve"
                contraction = contraction[:-1]
            result += contraction

        result = result.replace("'", '’')  # replace normal by typographic apostrophe
        return result

    @staticmethod
    @lru_cache(maxsize=1048576)
    def asciify(word: str) -> str:
        """Convert a word to its ASCII equivalent.

        Typographic apostrophes are replaced by normal ones and diacritical letters are
        replaced by their closest ASCII equivalents.
        """
        # pylint: disable=too-many-branches, too-many-statements
        result = word.replace('’', "'")  # ASCII-ify apostrophes

        if NON_ASCII_LETTERS_RE.search(result): # non-ASCII word
            letters = list(result)

            for i, letter in enumerate(letters):
                if ord(letter) < 128:
                    continue  # ASCII, nothing to do

                lower_letter = letter.lower()
                replacement = ''

                # vowels and semivowels
                if lower_letter in 'áàăâǎåäãąāả':
                    replacement = 'a'
                elif lower_letter in 'æ':
                    replacement = 'ae'
                elif lower_letter in 'éèĕêěëẽęēẻ':
                    replacement = 'e'
                elif lower_letter in 'íìĭîǐïĩįī':
                    replacement = 'i'
                elif lower_letter in 'óòŏôǒöõøǫōỏ':
                    replacement = 'o'
                elif lower_letter in 'œ':
                    replacement = 'oe'
                elif lower_letter in 'úùŭûǔůüũųūủ':
                    replacement = 'u'
                elif lower_letter in 'ýỳŷẙÿỹȳỷ':
                    replacement = 'y'
                # consonants
                elif lower_letter in 'ćĉčċç':
                    replacement = 'c'
                elif lower_letter in 'ďđ':
                    replacement = 'd'
                elif lower_letter in 'ǵğĝǧġḡ':
                    replacement = 'g'
                elif lower_letter in 'ĥȟḧħ':
                    replacement = 'h'
                elif lower_letter in 'ĵǰ':
                    replacement = 'j'
                elif lower_letter in 'ḱǩ':
                    replacement = 'k'
                elif lower_letter in 'ĺľłḹ':
                    replacement = 'l'
                elif lower_letter in 'ḿ':
                    replacement = 'm'
                elif lower_letter in 'ńǹňñ':
                    replacement = 'n'
                elif lower_letter in 'ṕ':
                    replacement = 'p'
                elif lower_letter in 'ŕř':
                    replacement = 'r'
                elif lower_letter in 'śŝšş':
                    replacement = 's'
                elif lower_letter in 'ß':
                    replacement = 'ss'
                elif lower_letter in 'ťẗ':
                    replacement = 't'
                elif lower_letter in 'ṽ':
                    replacement = 'v'
                elif lower_letter in 'ẃẁẘẅ':
                    replacement = 'w'
                elif lower_letter in 'ẍ':
                    replacement = 'x'
                elif lower_letter in 'źẑž':
                    replacement = 'z'

                if replacement:
                    if letter != lower_letter:  #  original was upper case
                        replacement = replacement.upper()
                    letters[i] = replacement

            result = ''.join(letters)
        return result

    @staticmethod
    def strip_generic_contraction(word: str) -> Tuple[str, str]:
        """Strip the genitive ("'s") and generic contractions (such as "'ve") from a word.

        Returns two values: actual word, contraction. If the word doesn't end in a genitive
        or contraction, the first return value will be identical to the argument and the
        second return value will be empty.

        Both simple and typographic apostrophes are recognized.
        """
        match = GENERIC_CONTRACTION_RE.search(word)
        if match:
            idx = match.start() + 1
            return word[:idx], word[idx:]
        else:
            return word, ''

    def fallback_lookup(self, lc_word: str) -> Optional[str]:
        """Fallback lookup if the regular lookup failed.

        'lc_word' must be a lower-case word.

        Checks if the word is a contraction that starts with an apostrophe (e.g. "'cause" or
        "'em"), as sometimes produced by spaCy, or if it ends in -in and a corresponding -ing
        entry exists (e.g. 'friggin' instead of 'frigging').
        """
        if lc_word.startswith("'"):
            result = self._dict.get(lc_word[1:])
            if isinstance(result, str):
                return "'" + result
        elif lc_word.endswith('in'):
            result = self._dict.get(lc_word + 'g')
            if isinstance(result, str) and result.endswith('g'):
                return result[:-1]

        return None

    @staticmethod
    def translate_pos(spacy_pos: str) -> str:
        """Translate a POS tag as used by spaCy into the form used in the dictionary."""
        if spacy_pos in ('AUX', 'VERB'):
            return 'v'
        elif spacy_pos in ('NOUN', 'PROPN'):
            return 'n'
        elif spacy_pos == 'ADJ':
            return 'aj'
        elif spacy_pos == 'ADV':
            return 'av'
        elif spacy_pos in ('ADP', 'PRON'):
            return 'prp'
        else:
            # Other POS tags shouldn't usually occur regarding our POS-tagged words,
            # but we use a reasonable default for cases where spaCy gets it wrong
            return 'v'

    def _find_pos_tagged_entry(self, entries: dict, pos: str,
                               tried_variants: List[str] = None) -> str:
        """Find the entry to use if several POS-tagged spellings exist for a word.

        'pos' is the POS tag as returned by spaCy (capitalized) or used in our dictionary
        (lower-case) - the former will be translated into the later if needed.

        'tried_variants' is used internally to prevent endless loops in case another POS tag
        has to be tried as fallback.
        """
        if pos and pos[0].isupper():
            pos = self.translate_pos(pos)
        if tried_variants is None:
            tried_variants = []

        tried_variants.append(pos)
        result = entries.get(pos)

        if result is None:
            fallback = POS_FALLBACKS.get(pos)

            if fallback is None or fallback in tried_variants:
                if fallback is None:
                    warn("No fallback found for POS tag '{}'; using last entry of {} as fallback"
                         .format(pos, entries))
                else:
                    warn("Trying POS tag '{}' after {} would loop; using last entry of {} as "
                         'fallback'.format(pos, tried_variants, entries))

                result = entries[sorted(entries.keys())[-1]]
            else:
                result = self._find_pos_tagged_entry(entries, fallback, tried_variants)

        return result

    def _restore_capitalization(self, converted: str, word: str) -> str:
        """Restore the capitalization of 'converted' to match that of 'word'."""
        if ALL_CAPS_RE.match(word):
            return converted.upper()  # ALL_CAPS

        if word in self._mixed_dict:
            return self._mixed_dict[word]  #  MixedCase

        if UPPER_CASE_RE.match(word):
            return converted[0].upper() + converted[1:]  # Capitalized

        return converted  # No change
