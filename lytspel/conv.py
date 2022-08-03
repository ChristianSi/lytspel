"""Defines the class that actually converts tradspell to Lytspel."""
# pylint: disable=c-extension-no-member, consider-using-f-string, no-else-return

from collections import Counter
from enum import Enum
from io import BytesIO
from os import path
import re
from sys import stderr, stdin, stdout
from typing import List, Sequence, Set, Tuple, Union
from warnings import warn
from zipfile import is_zipfile, ZipFile

from lxml import etree, html
import spacy

from .dict import ConvState, Dictionary, HANDLED_NON_ASCII_LETTERS
from .util import printmsg


TokenType = Enum('TokenType', 'Word Punctuation URL')  # pylint: disable=invalid-name

LOWERCASE_LETTER_PAT = '[a-z{}]'.format(HANDLED_NON_ASCII_LETTERS)

# Matches a word or non-word token.
TOKEN_RE = re.compile('(' + LOWERCASE_LETTER_PAT + "(?:['’]?" + LOWERCASE_LETTER_PAT +')*)',
                      re.IGNORECASE)

# Matches an arbitrary word (allowing non-ASCII and contractions).
# Words start with a letter, or with an apostrophe followed by a letter.
WORD_RE = re.compile("['’]?" + LOWERCASE_LETTER_PAT, re.IGNORECASE)

# Matches punctuation (non-alphabetic characters) allowed in a URL or email address.
# This is somewhat rough and also includes punctuation that often precedes or follows a URL,
# but it should work for nearly all common cases.
URL_PUNCTUATION_RE = re.compile(r"^[-1-9$_.+!*'(),;/?:@=&<>[\]#%^{|}~]+$")

# Heuristic used to detect the end of a sentence.
ENDS_SENTENCE_HEURISTIC_RE = re.compile(r'([.?!](\s*["\'“‘”’])?|:\s*["\'“‘]|>(\s*["\'“‘])?)\s*$')

# Matches a digit
DIGIT_RE = re.compile(r'\d')

# Matches the first non-whitespace characters that typically occur at the start of a XML or
# HTML file
XML_START_RE = re.compile('<[!?h]', re.IGNORECASE)

XHTML_NAMESPACE = '{http://www.w3.org/1999/xhtml}'

BLOCK_LEVEL_TAGS = frozenset(('address', 'article', 'aside', 'blockquote', 'canvas', 'dd', 'div',
                              'dl', 'dt', 'fieldset', 'figcaption', 'figure', 'footer', 'form',
                              'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'header', 'hr', 'li', 'main',
                              'nav', 'noscript', 'ol', 'output', 'p', 'pre', 'section', 'table',
                              'tfoot', 'ul', 'video'))


class Converter:
    """A converter from tradspell to Lytspel.

    Instances of this class are stateful and hence not threadsafe.
    """

    # Attributes that are shared across all instances, but loaded in the constructor to avoid
    # loading them in cases where they aren't needed (e.g. when this program is called with the
    # --help option)
    _dict = None
    _nlp = None

    def __init__(self, use_unknown_counter: bool = False) -> None:
        """Create a new instance.

        Parameters:

        * use_unknown_counter: whether unknown words should be counted
        """
        # Switch consulted during lookup and updated during conversion and tokenization
        self._at_sent_start = True

        # Optional counter of unknown words
        self._unknown_counter = None  # type: Counter
        if use_unknown_counter:
            self._unknown_counter = Counter()

        # Lazy initialization of static attributes
        if Converter._dict is None:
            Converter._dict = Dictionary()

        if Converter._nlp is None:
            try:
                # Load spaCy without any unnecessary components
                Converter._nlp = spacy.load('en_core_web_sm', disable=['parser', 'ner'])
            except OSError:
                printmsg('Downloading language model for the spaCy POS tagger\n'
                         "(don't worry, this will only happen once)")
                from spacy.cli import download  # pylint: disable=import-outside-toplevel
                download('en_core_web_sm')
                Converter._nlp = spacy.load('en_core_web_sm', disable=['parser', 'ner'])

    @staticmethod
    def tokenize_text(text: str) -> List[str]:
        """Tokenize a string, returning an array of words and punctuation.

        Words must start and end with a letter and may contain apostrophes.
        """
        if not text:
            return []  # Empty or None

        result = TOKEN_RE.split(text)
        # Remove first and/or last element if they are empty
        if result[0] == '':
            result.pop(0)
        if result[-1] == '':
            result.pop()
        return result

    @staticmethod
    def typed_tokenize(text: str) -> List[Tuple[TokenType, str]]:
        """Tokenize a string, returning a list of typed tokens.

        URL recognition is somewhat rough and a (URL, str) token may not always identify the
        exact start and end points of a URL correctly, but it should be sufficient to
        distinguish words within URLs (which should NOT be converted) from other words. The URL
        TokenType is also used for email addresses.
        """
        # pylint: disable=too-many-locals, too-many-nested-blocks
        raw_tokens = Converter.tokenize_text(text)
        raw_token_count = len(raw_tokens)
        result = []
        i = 0

        while i < raw_token_count:
            token = raw_tokens[i]

            if token.isalpha() and i + 4 < raw_token_count:
                # Check if this token starts a URL -- URLs have at least 5 raw tokens:
                # alphabetic URL_Punctuation alphabetic URL_Punctuation alphabetic,
                # optionally followed by more (URL_Punctuation, alphabetic) pairs
                next_first, next_second, next_third, next_fourth = raw_tokens[i+1 : i+5]
                if (next_second.isalpha() and next_fourth.isalpha()
                        and URL_PUNCTUATION_RE.match(next_first)
                        and URL_PUNCTUATION_RE.match(next_third)):
                    # Looks good so far, check whether additional tokens pairs belong to the URL
                    extra_token_count = 4
                    looks_good = True
                    period_count = int('.' in next_first) + int('.' in next_third)
                    slash_or_at_seen = ('/' in next_first or '@' in next_first or
                                        '/' in next_third or '@' in next_third)

                    while looks_good:
                        if i + extra_token_count + 2 < raw_token_count:
                            next_xth, next_yth = raw_tokens[i+extra_token_count+1
                                                            : i+extra_token_count+3]
                            if next_yth.isalpha() and URL_PUNCTUATION_RE.match(next_xth):
                                extra_token_count += 2
                                period_count += int('.' in next_xth)
                                if not slash_or_at_seen:
                                    slash_or_at_seen = '/' in next_xth or '@' in next_xth
                            else:
                                looks_good = False
                        else:
                            looks_good = False

                    # A valid URL (or email) must contain at least two '.' (between alphabetic
                    # elements) or '.' as well as '/' or '@'
                    if period_count >= 2 or (period_count and slash_or_at_seen):
                        # Assemble URL token
                        next_i = i + extra_token_count + 1
                        url_text = ''.join(raw_tokens[i:next_i])
                        result.append((TokenType.URL, url_text))
                        i = next_i
                        continue

            if WORD_RE.match(token):
                result.append((TokenType.Word, token))
            else:
                # Punctuation is anything that's not a word or a URL (also whitespace and numbers)
                result.append((TokenType.Punctuation, token))
            i += 1

        return result

    def text_looks_foreign(self, text: str) -> bool:
        """Check whether a text fragment looks like it's written in a foreign language.

        Returns True iff a majority of the words are unknown.
        """
        in_tokens = self.tokenize_text(text)
        known_words = 0
        unknown_words = 0

        for token in in_tokens:
            if WORD_RE.match(token):
                conv = Converter._dict.lookup(token, None, self._at_sent_start)

                if conv:
                    known_words += 1
                else:
                    unknown_words += 1

        return unknown_words > known_words

    @staticmethod
    def ends_sentence(token: str) -> bool:
        """Use some simple heuristics to determine whether a token seems to end a sentence.

        Returns True in one of three cases:

        * The tokens ends in a dot, question or exclamation mark, optionally followed by an
          opening or closing quote marks
        * It ends in a colon followed by an opening quote mark
        * It ends in '>' (quoted text marker), optionally followed by an opening quote mark

        Whitespace is ignored in all cases.
        """
        if not token:
            return False

        return bool(ENDS_SENTENCE_HEURISTIC_RE.search(token))

    def _update_at_sent_start(self, token: str) -> None:
        """Update the '_at_sent_start' attribute.

        Parameter:

        * token: the last read token which must be a non-word
        """
        local_value = self._at_sent_start
        if local_value:
            if DIGIT_RE.search(token):
                # Token contains digits, hence it is considered as first token of the new sentence
                local_value = False

        # Set to True if the token seems to end a sentence OR if the attribute was already True
        # (and not set to False by us due to encountering a number)
        self._at_sent_start = local_value or self.ends_sentence(token)

    def _convert_text_if_simple(self, text: str, test_if_foreign: bool = True,
                                starts_sent: bool = True) -> Union[str, ConvState]:
        """Convert a text fragment if doing so is possible without POS tagging.

        'starts_sent' is an optional attribute that specifies whether this text fragment is
        likely to start with a new sentence. If true, the '_at_sent_start' switch is set
        accordingly, otherwise it is left alone.

        The return value is either:

        * The converted text (a string)
        * ConvState.NLP_NEEDED if POS tagging (NLP) is needed)
        * ConvState.LOOKS_FOREIGN if 'test_if_foreign' is true and a majority of the words in
          the fragment are unknown
        """
        # pylint: disable=too-many-branches, too-many-locals
        orig_at_sent_start = self._at_sent_start  # Remember in case we have to restore it later

        if not text:
            return text  # Empty or None, nothing to do
        if starts_sent:
            self._at_sent_start = True

        typed_tokens = self.typed_tokenize(text)
        out_tokens = []  # type: List[str]
        lasttype = None
        lasttok = ''
        known_words = 0
        unknown_words = 0

        if self._unknown_counter is not None:
            local_unknown_counter = Counter()  # type: Counter

        for (toktype, token) in typed_tokens:
            if toktype is TokenType.Word:
                conv = Converter._dict.lookup(token, None, self._at_sent_start)

                if conv is ConvState.NLP_NEEDED:
                    self._at_sent_start = orig_at_sent_start
                    return ConvState.NLP_NEEDED
                elif isinstance(conv, str):
                    known_words += 1
                    out_tokens.append(conv)
                else:
                    unknown_words += 1
                    out_tokens.append(token)

                    if self._unknown_counter is not None:
                        actual_word = Converter._dict.strip_generic_contraction(token)[0]
                        if len(actual_word) > 1:
                            local_unknown_counter[actual_word] += 1

                self._at_sent_start = False
            else:
                # Not a word
                if token == '-' and lasttype is TokenType.Word:
                    # Check if this forms a hyphenated prefix with the preceding token, e.g. 're-'
                    conv = Converter._dict.lookup(lasttok + token, None, self._at_sent_start)

                    if isinstance(conv, str):
                        out_tokens[-1] = conv
                        # Decrement unknown word counter
                        if self._unknown_counter is not None and lasttok in local_unknown_counter:
                            local_unknown_counter[lasttok] -= 1
                    else:
                        out_tokens.append(token)
                else:
                    # Append non-word as is and check whether it terminates a sentence
                    out_tokens.append(token)
                    self._update_at_sent_start(token)

            lasttype = toktype
            lasttok = token

        if not test_if_foreign or unknown_words <= known_words:
            # We shouldn't make the foreign language test OR at least half of the words are known
            if self._unknown_counter is not None:
                self._unknown_counter += local_unknown_counter

            return ''.join(out_tokens)
        else:
            return ConvState.LOOKS_FOREIGN

    def convert_para(self, text: str, test_if_foreign: bool = True,
                     starts_sent: bool = True) -> str:
        """Convert a paragraph.

        'starts_sent' is an optional attribute that specifies whether this text fragment is likely
        to start with a new sentence. If true, the 'self._at_sent_start' switch is set accordingly,
        otherwise it is left alone.

        If 'test_if_foreign' is True and a majority of the words in the paragraph are unknown,
        the paragraph is assumed to be written in a foreign language and returned unchanged.
        """
        # pylint: disable=too-many-branches, too-many-locals, too-many-nested-blocks, too-many-statements
        if not text:
            if starts_sent:
                # Nothing to do, but we still update the global state (a new block-level HTML
                # element might have been opened)
                self._at_sent_start = True
            return text

        simple_result = self._convert_text_if_simple(text, test_if_foreign, starts_sent)

        if simple_result is ConvState.NLP_NEEDED:
            # We have to invoke spaCy for POS tagging (unless the text looks foreign)
            if self.text_looks_foreign(text):
                return text  # Return text unchanged

            doc = Converter._nlp(text)
            out_tokens = []  # type: List[str]
            lasttok = ''
            last_nonword = ''

            if starts_sent:
                self._at_sent_start = True

            for entry in doc:
                period_count = entry.text.count('.')
                # Sometimes spaCy doesn't recognize all word boundaries, hence we run our own
                # tokenizer on each of its entries (unless it looks like a URL or abbreviation)
                if period_count >= 2 or (period_count and ('/' in entry.text or '@' in entry.text)):
                    in_tokens = [entry.text]
                    looks_like_url = True
                else:
                    in_tokens = self.tokenize_text(entry.text)
                    looks_like_url = False

                # Glue contraction tokens such as "'ll" back together
                if len(in_tokens) >= 2 and in_tokens[0] in ("'", "’") and \
                        WORD_RE.match(in_tokens[1]):
                    in_tokens[0] += in_tokens[1]
                    in_tokens.pop(1)

                for idx, token in enumerate(in_tokens, start=1):
                    tail = ''

                    if idx == len(in_tokens) and entry.whitespace_:
                        tail = entry.whitespace_  # optional trailing whitespace after the entry

                    if token.lower() in ("n't", 'n’t'):
                        # SpaCy treats "n't" (as in "don't" etc.) as a separate word, but we look
                        # it up together with the preceding word because the joined pronunciation
                        # (and hence spelling) is sometimes different
                        if out_tokens:
                            out_tokens.pop()
                        token = lasttok + token

                        if self._unknown_counter is not None and lasttok in self._unknown_counter:
                            self._unknown_counter[lasttok] -= 1

                    if WORD_RE.match(token):
                        conv = Converter._dict.lookup(token, entry.pos_, self._at_sent_start)

                        if isinstance(conv, str):
                            out_tokens.append(conv)
                        elif conv is not None:
                            raise ValueError('lookup({}, {}) returned unexpected result: {}'
                                             .format(token, entry.pos_, conv))
                        else:
                            out_tokens.append(token)

                            if self._unknown_counter is not None and not looks_like_url:
                                actual_word = Converter._dict.strip_generic_contraction(token)[0]
                                if len(actual_word) > 1:
                                    self._unknown_counter[actual_word] += 1

                        self._at_sent_start = self.ends_sentence(tail)
                        last_nonword = ''
                    else:
                        # Not a word
                        if token == '-' and not tail and lasttok and WORD_RE.match(lasttok):
                            # Check if this forms a hyphenated prefix with the preceding token,
                            # e.g. 're-'
                            conv = Converter._dict.lookup(lasttok + token, entry.pos_,
                                                          self._at_sent_start)

                            if isinstance(conv, str):
                                out_tokens[-1] = conv
                                # Decrement unknown word counter
                                if self._unknown_counter is not None and \
                                        lasttok in self._unknown_counter:
                                    self._unknown_counter[lasttok] -= 1
                            elif conv is not None:
                                raise ValueError(
                                    'lookup({}, {}) returned unexpected result: {}'
                                    .format(token, entry.pos_, conv))
                            else:
                                out_tokens.append(token)
                                last_nonword = token
                        else:
                            # Append as is
                            out_tokens.append(token)

                            # We also consider the last non-word token for cases such as ‹: "›
                            # (colon followed by quote) which are considered two tokens by spaCy
                            self._update_at_sent_start(last_nonword + token + tail)
                            last_nonword = token

                    lasttok = token

                    # Append trailing whitespace to token, if any
                    if tail:
                        out_tokens[-1] += tail

            return ''.join(out_tokens)
        elif simple_result is ConvState.LOOKS_FOREIGN:
            return text  # Return text unchanged
        elif isinstance(simple_result, str):
            return simple_result
        else:
            raise ValueError('self._convert_text_if_simple returned unexpected result: {}'
                             .format(simple_result))

    @staticmethod
    def determine_file_type(filename: str) -> str:
        """Inspect the contents of a file to determine the likely file type.

        Returns either 'epub', 'html', 'txt', or None if the file is clearly not of any of
        these types. However, 'txt' is used as a fairly general fallback hence it's quite
        possible that a file labeled as 'txt' is actually something else
        """
        # Check if it's an epub file
        if is_zipfile(filename):
            with ZipFile(filename) as zin:
                bstr = b''

                try:
                    bstr = zin.read('mimetype')
                except KeyError:
                    pass  # Not an epub file

                if bstr.decode().startswith('application/epub+zip'):
                    return 'epub'
                else:
                    return None

        with open(filename, encoding='utf8') as file:
            for line in file:
                line = line.strip()

                if not line:
                    continue  # Empty line, inspect next one

                if XML_START_RE.match(line):
                    # File seems to start with a DOCTYPE or XML declaration, HTML comment or
                    # <html> tag
                    return 'html'
                else:
                    break

        return 'txt'

    # pylint: disable=protected-access

    @staticmethod
    def simple_tag(elem: etree._Element) -> str:
        """Return the tag name of an Element without the XHTML namespace, if used.

        If the element lives within the XHTML namespace, just the local name is returned,
        e.g. '{http://www.w3.org/1999/xhtml}img' becomes 'img'.

        If the element is a comment or PI, None is returned.

        In all other cases, the tag name is returned unchanged.
        """
        if isinstance(elem, (etree._Comment, etree._ProcessingInstruction)):
            return None

        tag = elem.tag

        if tag is None:
            return tag

        if tag.startswith(XHTML_NAMESPACE):
            return tag[len(XHTML_NAMESPACE):]
        else:
            return tag

    def convert_html_elem(self, elem: etree._Element) -> None:
        """Recursively convert an element in an HTML document and its children.

        Whether to convert textual content is decided on the level of block-level tags (such
        as 'h1', 'blockquote', 'p') that do NOT contain any directly nested block-level tags
        (e.g. if an 'ol' contains 'li' elements, the decision will be made for each of the
        latter independently, not for the whole 'ol'). If a large part of the text embedded
        in such an element seems to be in a foreign language, the whole element will NOT be
        converted.
        """
        # Check if we should made the foreign-language test as this level
        if self.simple_tag(elem) in BLOCK_LEVEL_TAGS:
            decide_on_conversion = True

            for child in elem:
                if self.simple_tag(child) in BLOCK_LEVEL_TAGS:
                    decide_on_conversion = False
                    break

            if decide_on_conversion:
                full_text = str(etree.XPath('string()')(elem))
                if self.text_looks_foreign(full_text):
                    # Skip this part of the document tree (doesn't seem to be English)
                    return

        # Convert immediate content (only block-level elements count as start of a new paragraph)
        elem.text = self.convert_para(elem.text, False,
                                      starts_sent=self.simple_tag(elem) in BLOCK_LEVEL_TAGS)

        # Convert child elements (except comments and those that don't contain normal text)
        for child in elem:
            if not (isinstance(child, (etree._Comment, etree._ProcessingInstruction))
                    or self.simple_tag(child) in ('script', 'style')):
                self.convert_html_elem(child)

            if child.tail:
                child.tail = self.convert_para(child.tail, False, starts_sent=False)

        # Convert a few textual attributes, if they are present
        for attrib in ('alt', 'title'):
            if attrib in elem.attrib and elem.attrib[attrib]:
                elem.attrib[attrib] = self.convert_para(elem.attrib[attrib])

    def convert_html_document(self, filename_or_bytes: Union[str, BytesIO]) -> bytes:
        """Convert and return an HTML or XHTML file.

        Returns an UTF-8 encoded bytestring.
        """
        doc = None
        is_xhtml = True

        # Try parsing as XHTML, if that doesn't work, parse as regular HTML
        try:
            doc = etree.parse(filename_or_bytes)
        except etree.XMLSyntaxError:
            doc = html.parse(filename_or_bytes)
            is_xhtml = False

        root = doc.getroot()
        title = root.find('.//title', namespaces=root.nsmap)
        body = root.find('body', namespaces=root.nsmap)

        if title is not None:
            title.text = self.convert_para(title.text)
        if body is not None:
            self.convert_html_elem(body)

        if is_xhtml:
            return etree.tostring(doc, encoding='utf8')
        else:
            return html.tostring(doc, encoding='utf8')

    def convert_xml_elem(self, elem: etree._Element) -> None:
        """Recursively convert an element in an XML document and its children.

        This function is only meant for elements that aren't (X)HTML. All textual content of
        the element and its child elements are converted (unless they look foreign), while
        all attribute values are left alone. In HTML, on the other hand, certain elements
        (such as 'script') are skipped and certain attributes (such as 'alt') are converted.

        Also, while in HTML the foreign-language test is made at the level of block-level tags
        (such as 'p' or 'li'), here every text fragment is tested independently.
        """
        # Convert immediate content
        elem.text = self.convert_para(elem.text)

        # Convert child elements (except comments and PIs)
        for child in elem:
            if not isinstance(child, (etree._Comment, etree._ProcessingInstruction)):
                self.convert_xml_elem(child)
            child.tail = self.convert_para(child.tail, starts_sent=False)

    def convert_xml_document(self, filename_or_bytes: Union[str, BytesIO]) -> bytes:
        """Convert and return an XML file.

        This function is only meant for documents that aren't (X)HTML.

        Returns an UTF-8 encoded bytestring.
        """
        doc = etree.parse(filename_or_bytes)
        self.convert_xml_elem(doc.getroot())
        return etree.tostring(doc, encoding='utf8')

    @staticmethod
    def _make_hrefs_absolute(items: Sequence[etree._Element], dirname: str) -> Sequence[
            etree._Element]:
        """Convert a list of XML elements with filenames from relative into absolute filenames.

        Each member of the 'items' sequence must have a 'href' attribute that will be
        modified accordingly.

        If dirname is empty, the original list is returned unchanged.
        """
        if dirname:
            for item in items:
                item.attrib['href'] = path.join(dirname, item.attrib['href'])

        return items

    def _find_epub_members_to_convert(self, zin: ZipFile) -> Tuple[Set[str], Set[str]]:
        """Find the files in an epub ZIP archive that should be converted.

        'zin' must be a ZipFile open for reading.

        Returns a tuple of two sets of absolute file names:

        * Set of HTML files to convert
        * Set of other XML files to convert (OPF files and the deprecated NCX files)
        """
        # Find the contents metafile
        bstr = zin.read('META-INF/container.xml')
        tree = etree.fromstring(bstr)
        # Usually there is just one OPF file, but multiple-rendition epubs have several ones
        opf_files = tree.xpath('n:rootfiles/n:rootfile/@full-path',
                               namespaces={'n': 'urn:oasis:names:tc:opendocument:xmlns:container'})
        absolute_items = []  # type: List[etree._Element]

        # Find the XML files that need conversion
        for opf_file in opf_files:
            bstr = zin.read(opf_file)
            tree = etree.fromstring(bstr)
            relative_items = tree.xpath('/p:package/p:manifest/p:item',
                                        namespaces={'p': 'http://www.idpf.org/2007/opf'})
            absolute_items += self._make_hrefs_absolute(relative_items, path.dirname(opf_file))

        html_files = set()
        ncx_files = set()

        for item in absolute_items:
            media_type = item.attrib['media-type']

            if media_type == 'application/xhtml+xml':
                html_files.add(item.attrib['href'])
            elif media_type == 'application/x-dtbncx+xml':
                # deprecated, but might occur in older epubs
                ncx_files.add(item.attrib['href'])

        other_xml_files = ncx_files.union(opf_files)
        return html_files, other_xml_files

    def convert_epub(self, filename: str, out_filename: str = None) -> None:
        """Convert an epub file.

        The 'out_filename' argument is optional; if omitted, a suitable name is generated by
        appending '-lytspel' before the file extension (if the input file is called 'FILE.epub',
        the output file will be called 'FILE-lytspel.epub').
        """
        if not out_filename:
            root, ext = path.splitext(filename)
            out_filename = '{}-lytspel{}'.format(root, ext)

        with ZipFile(filename, 'r') as zin:
            html_files, other_xml_files = self._find_epub_members_to_convert(zin)

            # Copy files to output archive, converting them if needed
            with ZipFile(out_filename, 'w') as zout:
                zout.comment = zin.comment  # Preserve the comment, if any

                for item in zin.infolist():
                    bstr = zin.read(item.filename)

                    if item.filename in html_files:
                        bio = BytesIO(bstr)
                        bstr = self.convert_html_document(bio)
                    if item.filename in other_xml_files:
                        bio = BytesIO(bstr)
                        bstr = self.convert_xml_document(bio)

                    zout.writestr(item, bstr)

        printmsg('Output written to {}'.format(out_filename))

    def convert_stdin_interactively(self) -> None:
        """Interactively convert plain text read from stdin, writing the output to stdout."""
        try:
            while True:
                # We use the input function here, since it allows pressing Arrow-Up to repeat
                # earlier inputs
                line = input()
                print(self.convert_para(line))
        except EOFError:
            return

    def convert_text_document(self, filename: str, out_filename: str = None) -> None:
        """Convert a plain text file.

        If 'filename' is '-', input is read from stdin.

        If `out_filename` is omitted, output will be written to stdout.
        """
        # pylint: disable=too-many-branches, consider-using-with
        if filename == '-':
            if stdin.isatty():
                self.convert_stdin_interactively()
                return

            infile = stdin
        else:
            infile = open(filename, encoding='utf8')

        if out_filename:
            outfile = open(out_filename, 'w', encoding='utf8')
        else:
            outfile = stdout

        para = ''

        for line in infile:
            line = line.rstrip()
            # Paragraphs are considered to be separated by empty lines. However, very long
            # lines (200+ chars) are considered paragraphs in their own right.
            if len(line) >= 200:  # stand-alone paragraph
                if para:
                    print(self.convert_para(para), file=outfile)
                    para = ''

                print(self.convert_para(line), file=outfile)
            elif line:            # regular line
                if para:
                    para += '\n'

                para += line
            else:                 # empty line
                if para:
                    print(self.convert_para(para), file=outfile)
                    para = ''

                print(file=outfile)

        # Convert final paragraph, if any
        if para:
            print(self.convert_para(para), file=outfile)

        if infile is not stdin:
            infile.close()

        if out_filename:
            outfile.close()

    def convert_file(self, filename: str, out_filename: str = None) -> None:
        """Convert the file with the specified name.

        Recognized file types are epub, HTML and plain text.

        If 'filename' is '-', input is read from stdin and assumed to be plain text.

        The 'out_filename' argument is optional; if omitted, HTML and text output will b
        written to stdout, while a suitable file name will be generated for epub.
        """
        if filename == '-':
            filetype = 'txt'
        else:
            filetype = self.determine_file_type(filename)

            if path.getsize(filename) / 1024 >= 256:
                printmsg('Converting {} -- this may take a while...'.format(filename))

        if filetype == 'txt':
            self.convert_text_document(filename, out_filename)
        elif filetype == 'html':
            bstr = self.convert_html_document(filename)

            if out_filename:
                with open(out_filename, 'wb') as outfile:
                    outfile.write(bstr)
                    outfile.write(b'\n')
            else:
                print(bstr.decode())

        elif filetype == 'epub':
            self.convert_epub(filename, out_filename)
        elif filetype is None:
            raise ValueError('Cannot convert {} (unsupported file type)'.format(filename))
        else:
            raise ValueError('Cannot convert {} (unexpected file type: {})'.format(
                filename, filetype))

    def _unify_case_differences(self) -> None:
        """Unify entries within the internal unknown word counter that differ only by case.

        Calling this method when this instance had been initialized with 'use_unknown_counter' =
        False will trigger an error.

        Any counts of capitalized words to the count of the lower-case variant if it exists,
        deleting the alternative entries. For example, Counter(hulla=3, Hulla=1, HULLA=1)
        becomes Counter(hulla=5).

        If there is no lower-case variant, other variants that differ in case will NOT be
        unified. For example, Counter(Hulla=1, HULLA=1) remains unchanged.
        """
        entries_to_delete = []

        for key, count in self._unknown_counter.items():
            if any(x.isupper() for x in key):
                lower = key.lower()

                if lower in self._unknown_counter:
                    self._unknown_counter[lower] += count
                    entries_to_delete.append(key)

        for entry in entries_to_delete:
            del self._unknown_counter[entry]

    def print_unknown_words(self, min_count: int = 1) -> None:
        """Writes the list of unknown words encountered in the converted texts to stderr.

        Only words encountered at least 'min_count' times are shown.

        Calling this method when this instance had been initialized with 'use_unknown_counter' =
        False will trigger a warning.
        """
        if self._unknown_counter is None:
            warn('print_unknown_words called on a Converter without an unknown_counter')
            return

        header_shown = False
        self._unify_case_differences()

        for key, count in sorted(self._unknown_counter.items(), key=lambda pair: pair[0].lower()):
            if count < min_count:
                continue

            output = '  ' + key

            if count > 1:
                output += ' ({}x)'.format(count)
            if not header_shown:
                print('Unknown words:', file=stderr)
                header_shown = True

            print(output, file=stderr)

        if not header_shown:
            if min_count <= 1:
                print('No unknown words', file=stderr)
            else:
                print('No unknown words (occurring {} times or more)'.format(min_count),
                      file=stderr)
