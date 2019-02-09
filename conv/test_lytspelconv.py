lc = __import__('lytspelconv')

# Convenience method

def lookup(word):
    return lc.lookup(word, '')


# Tests

def test_dict_filename():
    assert lc.dict_filename().endswith('data/lytspel-dict.csv')


def test_lookup_simple():
    assert lookup('blackbird') == 'blakburd'
    assert lookup('earthward') == 'urthwerd'
    assert lookup('shadows') == 'shadoas'
    assert lookup('sharp') == 'sharp'
    assert lookup('technology') == 'tec’noleji'
    assert lookup('tahiti') == 'ta’heeti'   # capitalized in dict
    assert lookup('notaword') == None


def test_lookup_case_restored():
    """Case should be restored."""
    assert lookup('Score') == 'Scoar'
    assert lookup('NETWORK') == 'NETWURK'
    assert lookup('POLITICAL') == 'PO’LITICL'
    assert lookup('JavaScript') == 'JaavaScript'
    assert lookup('McCain') == 'McCain'
    assert lookup('MeV') == 'MeV'
    assert lookup('ODed') == 'ODed'
    assert lookup("O'Donnell") == 'O’Donel'
    assert lookup('PCs') == 'PCs'
    assert lookup('PowerPC') == 'PowerPC'


def test_lookup_apostrophes():
    """Both normal and typographic apostrophes should be accepted."""
    assert lookup("we'd") == 'wi’d'
    assert lookup('we’d') == 'wi’d'
    assert lookup("it'll") == 'it’l'
    assert lookup('it’ll') == 'it’l'
    assert lookup("I'm") == 'y’m'
    assert lookup('I’m') == 'y’m'
    assert lookup("O'Connell") == 'O’Conel'
    assert lookup('O’Connell') == 'O’Conel'


def test_lookup_genitive_s():
    """'s (genitive or contraction) is handled correctly (normal and typographic forms)."""
    assert lookup("He's") == 'Hi’s'
    assert lookup("boyfriend's") == 'boyfrend’s'
    assert lookup('He’s') == 'Hi’s'
    assert lookup('boyfriend’s') == 'boyfrend’s'
    # Upper case words too
    assert lookup("HE'S") == 'HI’S'
    assert lookup("HE's") == 'HI’s'


def  test_lookup_contractions():
    assert lookup("'s") == '’s'
    assert lookup("’s") == '’s'
    assert lookup("'d") == '’d'
    assert lookup("’d") == '’d'
    assert lookup("'ll") == '’l'
    assert lookup("'re") == '’r'
    assert lookup("'ve") == '’v'
    assert lookup("’ve") == '’v'


def test_lookup_diacritics():
    assert lookup('café') == 'ca’fay'
    assert lookup('continuüm') == 'con’tiniuam'
    assert lookup('doppelgänger') == 'dopelganger'
    assert lookup('élite') == 'i’leet'
    assert lookup('Élite') == 'I’leet'
    assert lookup('épée') == 'ai’pay'
    assert lookup('ÉPÉE') == 'AI’PAY'
    assert lookup('hôtel') == 'hoa’tel'
    assert lookup('Hôtel') == 'Hoa’tel'
    assert lookup('mañana') == 'maa’nyaanaa'
    assert lookup('naïve') == 'naa’eev'
    assert lookup('œuvre') == 'uuvra'
    assert lookup('Œuvre') == 'Uuvra'
    assert lookup('tōfu') == 'toafu'


def test_lookup_redirects():
    """Test that redirects are resolved correctly."""
    assert lookup('Anglicised') == 'Anglicysd'
    assert lookup('barques') == 'barks'
    assert lookup('calking') == 'cauking'
    assert lookup('castors') == 'casters'
    assert lookup('Centre') == 'Senter'
    assert lookup('Compositae') == 'Com’posit'
    assert lookup('ESOP') == 'EESSOP'
    assert lookup('Ier') == 'Yr'
    assert lookup('OKs') == 'Oa’cays'
    assert lookup('paralyses') == 'parrelyses'
    assert lookup('prise') == 'prys'
    assert lookup('reanalyses') == 'ri’anelyses'


def test_lookup_nlp_needed_pos():
    """Test that lookup signals its need for a POS tag if one is necessary but not given."""
    assert lookup('increase') is lc.NLP_NEEDED
    assert lookup('misuse') is lc.NLP_NEEDED


def test_lookup_pos_tagged():
    """Test that POS-tagged words are lookup up correctly."""
    assert lc.lookup('increase', 'NOUN') == 'increess'
    assert lc.lookup('increase', 'VERB') == 'in’creess'
    assert lc.lookup('misuse', 'NOUN') == 'mis’iuss'
    assert lc.lookup('misuse', 'VERB') == 'mis’ius'


def test_tokenize_simple():
    assert '|'.join(lc.tokenize_text('This is a sentence.')) == 'This| |is| |a| |sentence|.'
    assert '|'.join(lc.tokenize_text('A sentence without final punctuation')) ==\
            'A| |sentence| |without| |final| |punctuation'
    assert '|'.join(lc.tokenize_text('Sentence with-some inner, punctuation;this should - not! — cause problems?Let    us\thope so!')) ==\
            'Sentence| |with|-|some| |inner|, |punctuation|;|this| |should| - |not|! — |cause| |problems|?|Let|    |us|\t|hope| |so|!'
    assert '|'.join(lc.tokenize_text('Some words in "double" and \'half quotation\' marks.')) ==\
            'Some| |words| |in| "|double|" |and| \'|half| |quotation|\' |marks|.'
    assert '|'.join(lc.tokenize_text('Some words in typographic “double” and ‘half quotation’ marks.')) ==\
            'Some| |words| |in| |typographic| “|double|” |and| ‘|half| |quotation|’ |marks|.'
    assert '|'.join(lc.tokenize_text('This is a sentence.')) == 'This| |is| |a| |sentence|.'
    assert '|'.join(lc.tokenize_text(':::sentence with leading punctuation:::')) ==\
            ':::|sentence| |with| |leading| |punctuation|:::'


def test_tokenize_contractions():
    assert '|'.join(lc.tokenize_text("Let's hope contractions are handled correctly wheresoe'er they'll occur, don't you think so, O'Connell?")) ==\
        "Let's| |hope| |contractions| |are| |handled| |correctly| |wheresoe'er| |they'll| |occur|, |don't| |you| |think| |so|, |O'Connell|?"
    assert '|'.join(lc.tokenize_text('Let’s hope contractions are handled correctly wheresoe’er they’ll occur, don’t you think so, O’Connell?')) ==\
        'Let’s| |hope| |contractions| |are| |handled| |correctly| |wheresoe’er| |they’ll| |occur|, |don’t| |you| |think| |so|, |O’Connell|?'
    assert '|'.join(lc.tokenize_text("He's happy to see my boyfriend's sister.")) ==\
        "He's| |happy| |to| |see| |my| |boyfriend's| |sister|."
    assert '|'.join(lc.tokenize_text('He’s happy to see my boyfriend’s sister.')) ==\
        'He’s| |happy| |to| |see| |my| |boyfriend’s| |sister|.'


def test_tokenize_diacritics():
    assert '|'.join(lc.tokenize_text('Mañana me and my naïve doppelgänger will eat tōfu in the café of an élite hôtel.')) ==\
      'Mañana| |me| |and| |my| |naïve| |doppelgänger| |will| |eat| |tōfu| |in| |the| |café| |of| |an| |élite| |hôtel|.'
    assert '|'.join(lc.tokenize_text('MAÑANA me and my naïve doppelGÄNGER will eat tōfu in the CAFÉ of an Élite Hôtel.')) ==\
      'MAÑANA| |me| |and| |my| |naïve| |doppelGÄNGER| |will| |eat| |tōfu| |in| |the| |CAFÉ| |of| |an| |Élite| |Hôtel|.'


def test_convert_para_simple():
    assert lc.convert_para('This is a sentence.') == 'Dhiss is a sentenss.'
    assert lc.convert_para('Some words in "double" and \'half quotation\' marks.') ==\
        'Sum wurds in "dubl" and \'haf quoa’taition\' marks.'
    assert lc.convert_para('Some words in typographic “double” and ‘half quotation’ marks.') ==\
        'Sum wurds in typo’grafic “dubl” and ‘haf quoa’taition’ marks.'


def test_convert_para_spacy_quotes():
    """Test that quote marks are also handled correctly if spaCy is invoked."""
    assert lc.convert_para('Who could object to some words in "double" and \'half quotation\' marks?') ==\
        'Hu cood ob’ject to sum wurds in "dubl" and \'haf quoa’taition\' marks?'
    assert lc.convert_para('Who could object to some words in typographic “double” and ‘half quotation’ marks?') ==\
        'Hu cood ob’ject to sum wurds in typo’grafic “dubl” and ‘haf quoa’taition’ marks?'
    assert lc.convert_para('Who could object to some words in typographic ‘‘fake double quotation’’ marks?') ==\
        'Hu cood ob’ject to sum wurds in typo’grafic ‘‘faik dubl quoa’taition’’ marks?'


def test_convert_para_contractions():
    assert lc.convert_para("Let's hope contractions are handled correctly wheresoe'er they'll occur, don't you think so, O'Connell?") ==\
        'Let’s hoap con’tractions ar handld ke’rectli wairso’air dhay’l o’cur, doan’t iu think so, O’Conel?'
    assert lc.convert_para('Let’s hope contractions are handled correctly wheresoe’er they’ll occur, don’t you think so, O’Connell?') ==\
        'Let’s hoap con’tractions ar handld ke’rectli wairso’air dhay’l o’cur, doan’t iu think so, O’Conel?'
    assert lc.convert_para("He's happy to see my boyfriend's sister.") ==\
        'Hi’s hapi to see my boyfrend’s sister.'
    assert lc.convert_para('He’s happy to see my boyfriend’s sister.') ==\
        'Hi’s hapi to see my boyfrend’s sister.'


def test_convert_para_pos_tagged():
    assert lc.convert_para('I did not object to the object.') == 'Y did not ob’ject to dhe object.'
    assert lc.convert_para('They were too close to the door to close it.') ==\
        'Dhay wur tu cloass to dhe doar to cloas it.'
    assert lc.convert_para('Before I mow the lawn let me place this grain in the mow.') ==\
        'Bi’foar y mo dhe laun let mi plaiss dhiss grain in dhe mow.'
    assert lc.convert_para('He thought it was time to present the present.') ==\
        'Hi thaut it wos tym to pri’sent dhe present.'
    assert lc.convert_para('I met an august man last August.') ==\
        'Y met an au’gust man last August.'
    assert lc.convert_para('To help with planting, the farmer taught his sow to sow.') ==\
        'To help widh planting, dhe farmer taut his sow to so.'
    assert lc.convert_para('The weather was beginning to affect his affect.') ==\
        'Dhe wedher wos bi’gining to a’fect his afect.'
    assert lc.convert_para('We must polish the Polish furniture.') ==\
        'Wi must polish dhe Poalish furnicher.'
    assert lc.convert_para('The dump was so full that it had to refuse more refuse.') ==\
        'Dhe dump wos so fool dhat it had to ri’fius moar refiuss.'
    assert lc.convert_para('I had to subject the subject to a series of tests.') ==\
        'Y had to sub’ject dhe subject to a seerees ov tests.'
    assert lc.convert_para('Don’t desert me here in the desert!') ==\
        'Doan’t di’surt mi heer in dhe desert!'
    assert lc.convert_para('The outright prohibition has caused smoking to be banned outright.') ==\
            'Dhe outryt proahi’bition has causd smoaking to bee band out’ryt.'


def test_convert_para_nt_contractions():
    assert lc.convert_para("Don't you think I won't do it, because I will!") ==\
            'Doan’t iu think y woan’t du it, bi’caus y wil!'
    assert lc.convert_para('Don’t you think I won’t do it, because I will!') ==\
            'Doan’t iu think y woan’t du it, bi’caus y wil!'
    assert lc.convert_para("You mustn't believe that they can't do such a thing.") ==\
            'Iu musn’t bi’leev dhat dhay can’t du such a thing.'
    assert lc.convert_para('You mustn’t believe that they can’t do such a thing.') ==\
            'Iu musn’t bi’leev dhat dhay can’t du such a thing.'

def test_convert_para_initial_nt():
    """Leading "n't", though certainly irregular, shouldn't cause problems.

    Rather, it should be treated as an unknown token and returned as is.
    """
    assert lc.convert_para("N't a good way to open a sentence.") ==\
            "N't a good way to oapen a sentenss."
    assert lc.convert_para('N’t a good way to open a sentence.') ==\
            'N’t a good way to oapen a sentenss.'


def test_convert_para_diacritics():
    assert lc.convert_para('Mañana me and my naïve doppelgänger will eat tōfu in the café of an élite hôtel.') ==\
      'Maa’nyaanaa mi and my naa’eev dopelganger wil eet toafu in dhe ca’fay ov an i’leet hoa’tel.'


def test_convert_i_case():
    """Test case of Converted "I" and contractions such as "I'd", "I'll".

    They should be capitalized at the start, but not in the middle of sentences.
    """
    assert lc.convert_para("I am capitalized at the start of sentences but I'm lower-case in the middle. I am still capitalized at the start.") ==\
            "Y am capitelysd at dhe start ov sentensses but y’m loer-caiss in dhe midl. Y am stil capitelysd at dhe start."
    assert lc.convert_para('I’d be capitalized at the start of sentences but I’ll be lower-case in the middle. I’d still be capitalized at the start.') ==\
            "Y’d bee capitelysd at dhe start ov sentensses but y’l bee loer-caiss in dhe midl. Y’d stil bee capitelysd at dhe start."
    assert lc.convert_para("I've seen that that works but I've not seen that this works. I've seen it now.") ==\
            "Y’v seen dhat dhat wurks but y’v not seen dhat dhiss wurks. Y’v seen it now."


def test_Is_word_simple():
    assert lc.is_word('a')
    assert lc.is_word('This')
    assert lc.is_word('sentence')
    assert lc.is_word("Let's")
    assert not lc.is_word('')
    assert not lc.is_word(' ')
    assert not lc.is_word('.')
    assert not lc.is_word('; ')
    assert not lc.is_word("'")
    assert not lc.is_word('’')
    assert not lc.is_word('“')


def test_Is_word_apostrophe():
    """Words starting with an apostrophe are recognized as such.

    SpaCy's tokenizer produces such words.
    """
    assert lc.is_word("'d")
    assert lc.is_word("'re")
    assert lc.is_word('’d"')
    assert lc.is_word('’re')


def test_is_word_diacritics():
    assert lc.is_word('œuvre')
    assert lc.is_word('élite')
    assert lc.is_word('épée')
    assert lc.is_word('doppelgänger')
    assert lc.is_word('Œuvre')
    assert lc.is_word('Élite')
    assert lc.is_word('ÉPÉE')
