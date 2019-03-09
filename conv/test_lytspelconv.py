from collections import Counter
from pytest import fixture

lc = __import__('lytspelconv')


# Fixtures

@fixture(scope="session")
def dict():
    return lc.Dictionary()

@fixture
def conv():
    return lc.Converter()


# Tests

def test_dict_filename(dict):
    assert dict._dict_filename().endswith('data/lytspel-dict.csv')


def test_lookup_simple(dict):
    assert dict.lookup('blackbird') == 'blakburd'
    assert dict.lookup('earthward') == 'urthwerd'
    assert dict.lookup('shadows') == 'shadoas'
    assert dict.lookup('sharp') == 'sharp'
    assert dict.lookup('technology') == 'tec’noleji'
    assert dict.lookup('tahiti') == 'ta’heeti'   # capitalized in dict
    assert dict.lookup('notaword') == None

def test_lookup_case_restored(dict):
    """Case should be restored."""
    assert dict.lookup('Score') == 'Scoar'
    assert dict.lookup('NETWORK') == 'NETWURK'
    assert dict.lookup('POLITICAL') == 'PO’LITICL'
    assert dict.lookup('JavaScript') == 'JaavaScript'
    assert dict.lookup('McCain') == 'McCain'
    assert dict.lookup('MeV') == 'MeV'
    assert dict.lookup('ODed') == 'ODed'
    assert dict.lookup("O'Donnell") == 'O’Donel'
    assert dict.lookup('PCs') == 'PCs'
    assert dict.lookup('PowerPC') == 'PowerPC'

def test_lookup_apostrophes(dict):
    """Both normal and typographic apostrophes should be accepted."""
    assert dict.lookup("we'd") == 'wi’d'
    assert dict.lookup('we’d') == 'wi’d'
    assert dict.lookup("it'll") == 'it’l'
    assert dict.lookup('it’ll') == 'it’l'
    assert dict.lookup("I'm").lower() == 'y’m'
    assert dict.lookup('I’m').lower() == 'y’m'
    assert dict.lookup("O'Connell") == 'O’Conel'
    assert dict.lookup('O’Connell') == 'O’Conel'

def test_lookup_genitive_s(dict):
    """'s (genitive or contraction) is handled correctly (normal and typographic forms)."""
    assert dict.lookup("He's") == 'Hi’s'
    assert dict.lookup("boyfriend's") == 'boyfrend’s'
    assert dict.lookup('He’s') == 'Hi’s'
    assert dict.lookup('boyfriend’s') == 'boyfrend’s'
    # Upper case words too
    assert dict.lookup("HE'S") == 'HI’S'
    assert dict.lookup("HE's") == 'HI’s'

def  test_lookup_contractions(dict):
    assert dict.lookup("'s") == '’s'
    assert dict.lookup("’s") == '’s'
    assert dict.lookup("'d") == '’d'
    assert dict.lookup("’d") == '’d'
    assert dict.lookup("'ll") == '’l'
    assert dict.lookup("'re") == '’r'
    assert dict.lookup("'ve") == '’v'
    assert dict.lookup("’ve") == '’v'

def test_lookup_diacritics(dict):
    assert dict.lookup('café') == 'ca’fay'
    assert dict.lookup('continuüm') == 'con’tiniuam'
    assert dict.lookup('doppelgänger') == 'dopelganger'
    assert dict.lookup('élite') == 'i’leet'
    assert dict.lookup('Élite') == 'I’leet'
    assert dict.lookup('épée') == 'ai’pay'
    assert dict.lookup('ÉPÉE') == 'AI’PAY'
    assert dict.lookup('hôtel') == 'hoa’tel'
    assert dict.lookup('Hôtel') == 'Hoa’tel'
    assert dict.lookup('mañana') == 'maa’nyaanaa'
    assert dict.lookup('naïve') == 'naa’eev'
    assert dict.lookup('œuvre') == 'uuvra'
    assert dict.lookup('Œuvre') == 'Uuvra'
    assert dict.lookup('tōfu') == 'toafu'

def test_lookup_redirects(dict):
    """Test that redirects are resolved correctly."""
    assert dict.lookup('Anglicised') == 'Anglicysd'
    assert dict.lookup('barques') == 'barks'
    assert dict.lookup('calking') == 'cauking'
    assert dict.lookup('castors') == 'casters'
    assert dict.lookup('Centre') == 'Senter'
    assert dict.lookup('Compositae') == 'Com’posit'
    assert dict.lookup('ESOP') == 'EESSOP'
    assert dict.lookup('Ier') == 'Yr'
    assert dict.lookup('OKs') == 'Oa’cays'
    assert dict.lookup('paralyses') == 'parrelyses'
    assert dict.lookup('prise') == 'prys'
    assert dict.lookup('reanalyses') == 'ri’anelyses'

def test_lookup_nlp_needed_pos(dict):
    """Test that dict.lookup signals its need for a POS tag if one is necessary but not given."""
    assert dict.lookup('increase') is lc.ConvState.NLP_NEEDED
    assert dict.lookup('misuse') is lc.ConvState.NLP_NEEDED

def test_lookup_pos_tagged(dict):
    """Test that POS-tagged words are looked up correctly."""
    assert dict.lookup('increase', 'NOUN') == 'increess'
    assert dict.lookup('increase', 'VERB') == 'in’creess'
    assert dict.lookup('misuse', 'NOUN') == 'mis’iuss'
    assert dict.lookup('misuse', 'VERB') == 'mis’ius'

def test_lookup_hyphenated_prefix(dict):
    assert dict.lookup('re-') == 'ri-'
    assert dict.lookup('Re-') == 'Ri-'
    assert dict.lookup('RE-') == 'RI-'
    assert dict.lookup('de-') == 'di-'
    assert dict.lookup('meta-') == 'mete-'
    assert dict.lookup('paleo-') == 'pailio-'
    assert dict.lookup('Paleo-') == 'Pailio-'
    assert dict.lookup('PALEO-') == 'PAILIO-'

def test_lookup_us(dict):
    """Special: 'US' should remain unchanged if it's a capitalized abbreviation."""
    assert dict.lookup('us') == 'uss'
    assert dict.lookup('Us') == 'Uss'
    assert dict.lookup('US') == 'US'
    assert dict.lookup('US') == 'US'
    assert dict.lookup("US's") == 'US’s'
    assert dict.lookup('US’s') == 'US’s'


def test_tokenize_simple(conv):
    assert '|'.join(conv.tokenize_text('This is a sentence.')) == 'This| |is| |a| |sentence|.'
    assert '|'.join(conv.tokenize_text('A sentence without final punctuation')) ==\
            'A| |sentence| |without| |final| |punctuation'
    assert '|'.join(conv.tokenize_text('Sentence with-some inner, punctuation;this should - not! — cause problems?Let    us\thope so!')) ==\
            'Sentence| |with|-|some| |inner|, |punctuation|;|this| |should| - |not|! — |cause| |problems|?|Let|    |us|\t|hope| |so|!'
    assert '|'.join(conv.tokenize_text('Some words in "double" and \'half quotation\' marks.')) ==\
            'Some| |words| |in| "|double|" |and| \'|half| |quotation|\' |marks|.'
    assert '|'.join(conv.tokenize_text('Some words in typographic “double” and ‘half quotation’ marks.')) ==\
            'Some| |words| |in| |typographic| “|double|” |and| ‘|half| |quotation|’ |marks|.'
    assert '|'.join(conv.tokenize_text('This is a sentence.')) == 'This| |is| |a| |sentence|.'
    assert '|'.join(conv.tokenize_text(':::sentence with leading punctuation:::')) ==\
            ':::|sentence| |with| |leading| |punctuation|:::'

def test_tokenize_contractions(conv):
    assert '|'.join(conv.tokenize_text("Let's hope contractions are handled correctly wheresoe'er they'll occur, don't you think so, O'Connell?")) ==\
        "Let's| |hope| |contractions| |are| |handled| |correctly| |wheresoe'er| |they'll| |occur|, |don't| |you| |think| |so|, |O'Connell|?"
    assert '|'.join(conv.tokenize_text('Let’s hope contractions are handled correctly wheresoe’er they’ll occur, don’t you think so, O’Connell?')) ==\
        'Let’s| |hope| |contractions| |are| |handled| |correctly| |wheresoe’er| |they’ll| |occur|, |don’t| |you| |think| |so|, |O’Connell|?'
    assert '|'.join(conv.tokenize_text("He's happy to see my boyfriend's sister.")) ==\
        "He's| |happy| |to| |see| |my| |boyfriend's| |sister|."
    assert '|'.join(conv.tokenize_text('He’s happy to see my boyfriend’s sister.')) ==\
        'He’s| |happy| |to| |see| |my| |boyfriend’s| |sister|.'

def test_tokenize_diacritics(conv):
    assert '|'.join(conv.tokenize_text('Mañana me and my naïve doppelgänger will eat tōfu in the café of an élite hôtel.')) ==\
      'Mañana| |me| |and| |my| |naïve| |doppelgänger| |will| |eat| |tōfu| |in| |the| |café| |of| |an| |élite| |hôtel|.'
    assert '|'.join(conv.tokenize_text('MAÑANA me and my naïve doppelGÄNGER will eat tōfu in the CAFÉ of an Élite Hôtel.')) ==\
      'MAÑANA| |me| |and| |my| |naïve| |doppelGÄNGER| |will| |eat| |tōfu| |in| |the| |CAFÉ| |of| |an| |Élite| |Hôtel|.'

def test_tokenize_dates_and_numbers(conv):
    assert '|'.join(conv.tokenize_text('On 11 Feb. 2019 I wrote a test.')) ==\
            'On| 11 |Feb|. 2019 |I| |wrote| |a| |test|.'
    assert '|'.join(conv.tokenize_text('33 divided by 2 is 16.5.')) ==\
            '33 |divided| |by| 2 |is| 16.5.'


def test_convert_para_simple(conv):
    assert conv.convert_para('This is a sentence.') == 'Dhiss is a sentenss.'
    assert conv.convert_para('Some words in "double" and \'half quotation\' marks.') ==\
        'Sum wurds in "dubl" and \'haf quoa’taition\' marks.'
    assert conv.convert_para('Some words in typographic “double” and ‘half quotation’ marks.') ==\
        'Sum wurds in typo’grafic “dubl” and ‘haf quoa’taition’ marks.'

def test_convert_para_spacy_quotes(conv):
    """Test that quote marks are also handled correctly if spaCy is invoked."""
    assert conv.convert_para('Who could object to some words in "double" and \'half quotation\' marks?') ==\
        'Hu cood ob’ject to sum wurds in "dubl" and \'haf quoa’taition\' marks?'
    assert conv.convert_para('Who could object to some words in typographic “double” and ‘half quotation’ marks?') ==\
        'Hu cood ob’ject to sum wurds in typo’grafic “dubl” and ‘haf quoa’taition’ marks?'
    assert conv.convert_para('Who could object to some words in typographic ‘‘fake double quotation’’ marks?') ==\
        'Hu cood ob’ject to sum wurds in typo’grafic ‘‘faik dubl quoa’taition’’ marks?'

def test_convert_para_contractions(conv):
    assert conv.convert_para("Let's hope contractions are handled correctly wheresoe'er they'll occur, don't you think so, O'Connell?") ==\
        'Let’s hoap con’tractions ar handld ke’rectli wairso’air dhay’l o’cur, doan’t iu think so, O’Conel?'
    assert conv.convert_para('Let’s hope contractions are handled correctly wheresoe’er they’ll occur, don’t you think so, O’Connell?') ==\
        'Let’s hoap con’tractions ar handld ke’rectli wairso’air dhay’l o’cur, doan’t iu think so, O’Conel?'
    assert conv.convert_para("He's happy to see my boyfriend's sister.") ==\
        'Hi’s hapi to see my boyfrend’s sister.'
    assert conv.convert_para('He’s happy to see my boyfriend’s sister.') ==\
        'Hi’s hapi to see my boyfrend’s sister.'

def test_convert_para_pos_tagged(conv):
    assert conv.convert_para('I did not object to the object.') == 'Y did not ob’ject to dhe object.'
    assert conv.convert_para('They were too close to the door to close it.') ==\
        'Dhay wur tu cloass to dhe doar to cloas it.'
    assert conv.convert_para('Before I mow the lawn let me place this grain in the mow.') ==\
        'Bi’foar y mo dhe laun let mi plaiss dhiss grain in dhe mow.'
    assert conv.convert_para('He thought it was time to present the present.') ==\
        'Hi thaut it wos tym to pri’sent dhe present.'
    assert conv.convert_para('I met an august man last August.') ==\
        'Y met an au’gust man last August.'
    assert conv.convert_para('To help with planting, the farmer taught his sow to sow.') ==\
        'To help widh planting, dhe farmer taut his sow to so.'
    assert conv.convert_para('The weather was beginning to affect his affect.') ==\
        'Dhe wedher wos bi’gining to a’fect his afect.'
    assert conv.convert_para('We must polish the Polish furniture.') ==\
        'Wi must polish dhe Poalish furnicher.'
    assert conv.convert_para('The dump was so full that it had to refuse more refuse.') ==\
        'Dhe dump wos so fool dhat it had to ri’fius moar refiuss.'
    assert conv.convert_para('I had to subject the subject to a series of tests.') ==\
        'Y had to sub’ject dhe subject to a seerees ov tests.'
    assert conv.convert_para('Don’t desert me here in the desert!') ==\
        'Doan’t di’surt mi heer in dhe desert!'
    assert conv.convert_para('The outright prohibition has caused smoking to be banned outright.') ==\
            'Dhe outryt proahi’bition has causd smoaking to bee band out’ryt.'

def test_convert_para_nt_contractions(conv):
    assert conv.convert_para("Don't you think I won't do it, because I will!") ==\
            'Doan’t iu think y woan’t du it, bi’caus y wil!'
    assert conv.convert_para('Don’t you think I won’t do it, because I will!') ==\
            'Doan’t iu think y woan’t du it, bi’caus y wil!'
    assert conv.convert_para("You mustn't believe that they can't do such a thing.") ==\
            'Iu musn’t bi’leev dhat dhay can’t du such a thing.'
    assert conv.convert_para('You mustn’t believe that they can’t do such a thing.') ==\
            'Iu musn’t bi’leev dhat dhay can’t du such a thing.'

def test_convert_para_initial_nt(conv):
    """Leading "n't", though certainly irregular, shouldn't cause problems.

    Rather, it should be treated as an unknown token and returned as is.
    """
    assert conv.convert_para("N't a good way to open a sentence.") ==\
            "N't a good way to oapen a sentenss."
    assert conv.convert_para('N’t a good way to open a sentence.') ==\
            'N’t a good way to oapen a sentenss.'

def test_convert_para_diacritics(conv):
    assert conv.convert_para('Mañana me and my naïve doppelgänger will eat tōfu in the café of an élite hôtel.') ==\
      'Maa’nyaanaa mi and my naa’eev dopelganger wil eet toafu in dhe ca’fay ov an i’leet hoa’tel.'


def test_convert_i_case(conv):
    """Test case of converted "I" and contractions such as "I'd", "I'll".

    They should be capitalized at the start, but not in the middle of sentences.
    """
    assert conv.convert_para("I am capitalized at the start of sentences but I'm lower-case in the middle. I am still capitalized at the start.") ==\
            "Y am capitelysd at dhe start ov sentensses but y’m loer-caiss in dhe midl. Y am stil capitelysd at dhe start."
    assert conv.convert_para('I’d be capitalized at the start of sentences but I’ll be lower-case in the “middle”! I’d still be capitalized at the start.') ==\
            'Y’d bee capitelysd at dhe start ov sentensses but y’l bee loer-caiss in dhe “midl”! Y’d stil bee capitelysd at dhe start.'
    assert conv.convert_para("I've seen that that works but I've not seen whether this works? I've seen it now.") ==\
            'Y’v seen dhat dhat wurks but y’v not seen wedher dhiss wurks? Y’v seen it now.'

def test_convert_i_case_quotes(conv):
    """Test "I" case conversion after quote marks."""
    assert conv.convert_para('Opening quote marks don\'t hurt, I guess. "I am still capitalized."') ==\
            'Oapening quoat marks doan’t hurt, y gess. "Y am stil capitelysd."'
    assert conv.convert_para('Opening quote marks don’t hurt, I guess. “I am still capitalized.”') ==\
            'Oapening quoat marks doan’t hurt, y gess. “Y am stil capitelysd.”'
    assert conv.convert_para('"Closing trailing quote marks don\'t hurt, I hope?" I\'m still capitalized.') ==\
            '"Cloasing trailing quoat marks doan’t hurt, y hoap?" Y’m stil capitelysd.'
    assert conv.convert_para('“Closing trailing quote marks don’t hurt, I hope!” I’m still capitalized.') ==\
            '“Cloasing trailing quoat marks doan’t hurt, y hoap!” Y’m stil capitelysd.'
    assert conv.convert_para("Closing leading quote marks don't hurt, I 'hope'. I am still capitalized.") ==\
            "Cloasing leeding quoat marks doan’t hurt, y 'hoap'. Y am stil capitelysd."
    assert conv.convert_para('Closing leading quote marks don’t hurt, I ‘hope’! I am still capitalized.') ==\
            'Cloasing leeding quoat marks doan’t hurt, y ‘hoap’! Y am stil capitelysd.'

def test_convert_i_case_colon(conv):
    """Test "I" case conversion after a colon.

    A quoted phrase after a colon is assumed to start a new sentence (capitalized).
    In all other cases, the preceding sentence is assumed to continue (lower-case).
    """
    assert conv.convert_para('After a colon: I should not be capitalized.') ==\
            'After a coalen: y shood not bee capitelysd.'
    assert conv.convert_para('I said: "I hope I\'ll be capitalized now."') ==\
            'Y sed: "Y hoap y’l bee capitelysd now."'
    assert conv.convert_para('I said: “I hope I’ll be capitalized now.”') ==\
            'Y sed: “Y hoap y’l bee capitelysd now.”'
    assert conv.convert_para('I said: ‘I hope I’ll be capitalized now.’') ==\
            'Y sed: ‘Y hoap y’l bee capitelysd now.’'

def test_convert_i_case_dates_and_numbers(conv):
    """Test "I" case conversion after dates and numbers."""
    assert conv.convert_para('On 11 Feb. 2019 I wrote a test.') ==\
            'On 11 Feb. 2019 y roat a test.'
    assert conv.convert_para('Even after a fraction such as 3.5 I should still be lower-case.') ==\
            'Eeven after a fraction such as 3.5 y shood stil bee loer-caiss.'

def test_convert_i_case_spacy(conv):
    """Test that "I" case conversion also works if Spacy is invoked."""
    assert conv.convert_para("I estimate that I am capitalized at the start of sentences but that I'm lower-case in the middle. I am still capitalized at the start.") ==\
            "Y estimait dhat y am capitelysd at dhe start ov sentensses but dhat y’m loer-caiss in dhe midl. Y am stil capitelysd at dhe start."
    assert conv.convert_para('Opening quote marks don’t hurt, I estimate. “I am still capitalized.”') ==\
            'Oapening quoat marks doan’t hurt, y estimait. “Y am stil capitelysd.”'
    assert conv.convert_para('"Closing trailing quote marks don\'t hurt, I estimate?" I\'m still capitalized.') ==\
            '"Cloasing trailing quoat marks doan’t hurt, y estimait?" Y’m stil capitelysd.'
    assert conv.convert_para('Closing leading quote marks don’t hurt, ‘I’d estimate’! I am still capitalized.') ==\
            'Cloasing leeding quoat marks doan’t hurt, ‘y’d estimait’! Y am stil capitelysd.'
    assert conv.convert_para('After a colon: I should not be capitalized, I’d estimate.') ==\
            'After a coalen: y shood not bee capitelysd, y’d estimait.'
    assert conv.convert_para('I said: “I estimate I’ll be capitalized now.”') ==\
            'Y sed: “Y estimait y’l bee capitelysd now.”'
    assert conv.convert_para('I estimate that around 11 Feb. 2019 I wrote a test.') ==\
            'Y estimait dhat e’round 11 Feb. 2019 y roat a test.'
    assert conv.convert_para('Even after a fraction such as 3.5 I should still be lower-case, I estimate.') ==\
            'Eeven after a fraction such as 3.5 y shood stil bee loer-caiss, y estimait.'


def test_convert_hyphenated_prefixes(conv):
    assert conv.convert_para('I re-sent the file to you last night.') ==\
        'Y ri-sent dhe fyl to iu last nyt.'
    # Test that it works with spaCy too
    assert conv.convert_para('I estimate that I have to re-press the shirt.') ==\
        'Y estimait dhat y hav to ri-press dhe shurt.'
    # Also if capitalized
    assert conv.convert_para('The Re-Creation of a Lost Artwork') ==\
        'Dhe Ri-Cri’aition ov a Lost Artwurk'
    # Nothing should happen in other cases
    assert conv.convert_para('I talked with her re the case.') ==\
        'Y taukd widh hur ree dhe caiss.'
    assert conv.convert_para('Re: Your mail') ==\
        'Ree: Iur mail'
    # Also test some other prefix
    assert conv.convert_para('She de-emphasized our differences.') ==\
            'Shi di-emfecysd our diferensses.'
    assert conv.convert_para('The Paleo-Americans arrived in the Americas a long, long time ago.') ==\
        'Dhe Pailio-A’merricans e’ryvd in dhe A’merricas a long, long tym a’go.'

def test_convert_other_hyphens(conv):
    """Test that hyphens between words don't cause problems."""
    # Hyphens between independent words
    assert conv.convert_para('We need a state-of-the-art solution.') ==\
        'Wi need a stait-ov-dhe-art so’luution.'
    # Not a prefix, since there is whitespace after the hyphen
    assert conv.convert_para('I’m contacting you re- you might have guessed it -your mail.') ==\
        'Y’m contacting iu ree- iu myt hav gessd it -iur mail.'
    # Likewise if another hyphen follows
    assert conv.convert_para('I’m contacting you re--you might have guessed it--your mail.') ==\
        'Y’m contacting iu ree--iu myt hav gessd it--iur mail.'


def test_is_word_simple(conv):
    assert conv.is_word('a')
    assert conv.is_word('This')
    assert conv.is_word('sentence')
    assert conv.is_word("Let's")
    assert not conv.is_word('')
    assert not conv.is_word(' ')
    assert not conv.is_word('.')
    assert not conv.is_word('; ')
    assert not conv.is_word("'")
    assert not conv.is_word('’')
    assert not conv.is_word('“')

def test_is_word_apostrophe(conv):
    """Words starting with an apostrophe are recognized as such.

    SpaCy's tokenizer produces such words.
    """
    assert conv.is_word("'d")
    assert conv.is_word("'re")
    assert conv.is_word('’d"')
    assert conv.is_word('’re')

def test_is_word_diacritics(conv):
    assert conv.is_word('œuvre')
    assert conv.is_word('élite')
    assert conv.is_word('épée')
    assert conv.is_word('doppelgänger')
    assert conv.is_word('Œuvre')
    assert conv.is_word('Élite')
    assert conv.is_word('ÉPÉE')


def test_unify_case_differences_basic(conv):
    """Test that the counter works as it should."""
    conv._unknown_counter = Counter(hulla=3, Hulla=1, HULLA=1)
    conv._unify_case_differences()
    assert conv._unknown_counter == Counter(hulla=5)

def test_unify_case_differences_empty(conv):
    """Empty counter should not change."""
    conv._unknown_counter = Counter()
    conv._unify_case_differences()
    assert conv._unknown_counter == Counter()

def test_unify_case_differences_no_common_lower_case(conv):
    """Counter should not change if there is no common lower-case version."""
    conv._unknown_counter = Counter(Hulla=1, HULLA=1)
    conv._unify_case_differences()
    assert conv._unknown_counter == Counter(Hulla=1, HULLA=1)

def test_unify_case_differences_different_entries(conv):
    """Counter should not change if all entries differ even ignoring case."""
    conv._unknown_counter = Counter(alpha=1, Beta=2, GAMMA=3, delta=4)
    conv._unify_case_differences()
    assert conv._unknown_counter == Counter(alpha=1, Beta=2, GAMMA=3, delta=4)
