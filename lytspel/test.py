"""Unit tests for the Lytspel converter."""

# pylint: disable=invalid-name, line-too-long, missing-docstring, redefined-outer-name, protected-access
# Note: invalid-name disabled since pylint limits function names to 30 chars.

from collections import Counter
from pytest import fixture

from . import Converter
from .conv import WORD_RE
from .dict import ConvState, Dictionary

# Fixtures

@fixture(scope='session')
def dct():
    return Dictionary()

@fixture(scope='session')
def conv():
    return Converter()

@fixture
def is_word():
    """'is_word' was formerly a function.

    But it has been replaced with a direct regex call for efficiency."""
    return WORD_RE.match

# Tests

def test_lookup_simple(dct):
    assert dct.lookup('blackbird') == 'blakburd'
    assert dct.lookup('earthward') == 'urthwerd'
    assert dct.lookup('shadows') == 'shadoas'
    assert dct.lookup('sharp') == 'sharp'
    assert dct.lookup('technology') == 'tecnoleji'
    assert dct.lookup('tahiti') == 'taheeti'   # capitalized in dict
    assert dct.lookup('notaword') is None

def test_lookup_case_restored(dct):
    """Case should be restored."""
    assert dct.lookup('Score') == 'Scor'
    assert dct.lookup('NETWORK') == 'NETWURK'
    assert dct.lookup('POLITICAL') == 'POLITICL'
    assert dct.lookup('JavaScript') == 'JaavaScript'
    assert dct.lookup('McCain') == 'McCain'
    assert dct.lookup('MeV') == 'MeV'
    assert dct.lookup('ODed') == 'ODed'
    assert dct.lookup("O'Donnell") == 'O’Donel'
    assert dct.lookup('PCs') == 'PCs'
    assert dct.lookup('PowerPC') == 'PowerPC'

def test_lookup_apostrophes(dct):
    """Both normal and typographic apostrophes should be accepted."""
    assert dct.lookup("we'd") == 'wi’d'
    assert dct.lookup('we’d') == 'wi’d'
    assert dct.lookup("it'll") == 'it’l'
    assert dct.lookup('it’ll') == 'it’l'
    assert dct.lookup("I'm").lower() == 'y’m'
    assert dct.lookup('I’m').lower() == 'y’m'
    assert dct.lookup("O'Connell") == 'O’Conel'
    assert dct.lookup('O’Connell') == 'O’Conel'

def test_lookup_genitive_s(dct):
    """'s (genitive or contraction) is handled correctly (normal and typographic forms)."""
    assert dct.lookup("He's") == 'Hi’s'
    assert dct.lookup("boyfriend's") == 'boyfrend’s'
    assert dct.lookup('He’s') == 'Hi’s'
    assert dct.lookup('boyfriend’s') == 'boyfrend’s'
    # Upper case words too
    assert dct.lookup("HE'S") == 'HI’S'
    assert dct.lookup("HE's") == 'HI’s'

def test_lookup_contractions(dct):
    assert dct.lookup("'s") == '’s'
    assert dct.lookup("’s") == '’s'
    assert dct.lookup("'d") == '’d'
    assert dct.lookup("’d") == '’d'
    assert dct.lookup("'ll") == '’l'
    assert dct.lookup("'re") == '’r'
    assert dct.lookup("'ve") == '’v'
    assert dct.lookup("’ve") == '’v'

def test_lookup_nonstandard_contractions(dct):
    assert dct.lookup('Peter’d') == 'Peeter’d'
    assert dct.lookup('PETER’D') == 'PEETER’D'
    assert dct.lookup("what'll") == 'whot’l'
    assert dct.lookup("What'll") == 'Whot’l'
    assert dct.lookup('What’Re') == 'Whot’R'
    assert dct.lookup("where've") == 'whair’v'
    assert dct.lookup("WHERE'VE") == 'WHAIR’V'

def test_lookup_diacritics(dct):
    assert dct.lookup('café') == 'cafay'
    assert dct.lookup('continuüm') == 'contínueem'
    assert dct.lookup('doppelgänger') == 'dopelganger'
    assert dct.lookup('élite') == 'ileet'
    assert dct.lookup('Élite') == 'Ileet'
    assert dct.lookup('épée') == 'aipáy'
    assert dct.lookup('ÉPÉE') == 'AIPÁY'
    assert dct.lookup('hôtel') == 'hoatél'
    assert dct.lookup('Hôtel') == 'Hoatél'
    assert dct.lookup('mañana') == 'maanyáanaa'
    assert dct.lookup('naïve') == 'naaéev'
    assert dct.lookup('œuvre') == 'uuvra'
    assert dct.lookup('Œuvre') == 'Uuvra'
    assert dct.lookup('tōfu') == 'toafu'

def test_lookup_redirects(dct):
    """Test that redirects are resolved correctly."""
    assert dct.lookup('Anglicised') == 'Ánglicysd'
    assert dct.lookup('barques') == 'barks'
    assert dct.lookup('calking') == 'cauking'
    assert dct.lookup('castors') == 'casters'
    assert dct.lookup('Centre') == 'Senter'
    assert dct.lookup('aesthetician') == 'esthitition'
    assert dct.lookup('ESOP') == 'EESSOP'
    assert dct.lookup('Ier') == 'Yr'
    assert dct.lookup('OKs') == 'Oacáys'
    assert dct.lookup('paralyses') == 'parrelyses'
    assert dct.lookup('prise') == 'prys'
    assert dct.lookup('reanalyses') == 'riánelyses'

def test_lookup_nlp_needed_pos(dct):
    """Test that lookup signals its need for a POS tag if one is necessary but not given."""
    assert dct.lookup('increase') is ConvState.NLP_NEEDED
    assert dct.lookup('misuse') is ConvState.NLP_NEEDED

def test_lookup_pos_tagged(dct):
    """Test that POS-tagged words are looked up correctly."""
    assert dct.lookup('increase', 'NOUN') == 'íncreess'
    assert dct.lookup('increase', 'VERB') == 'increess'
    assert dct.lookup('misuse', 'NOUN') == 'misuess'
    assert dct.lookup('misuse', 'VERB') == 'misues'

def test_lookup_hyphenated_prefix(dct):
    assert dct.lookup('re-') == 'ri-'
    assert dct.lookup('Re-') == 'Ri-'
    assert dct.lookup('RE-') == 'RI-'
    assert dct.lookup('de-') == 'di-'
    assert dct.lookup('meta-') == 'mete-'
    assert dct.lookup('paleo-') == 'palio-'
    assert dct.lookup('Paleo-') == 'Palio-'
    assert dct.lookup('PALEO-') == 'PALIO-'

def test_lookup_us(dct):
    """A few capitalized abbreviation such as 'US' should remain unchanged."""
    assert dct.lookup('us') == 'uss'
    assert dct.lookup('Us') == 'Uss'
    assert dct.lookup('US') == 'US'
    assert dct.lookup('US') == 'US'
    assert dct.lookup("US's") == 'US’s'
    assert dct.lookup('US’s') == 'US’s'

def test_lookup_in(dct):
    """Accept -in (usually followed by a contraction marker) as an alternative to -ing."""
    assert dct.lookup('friggin') == 'frigin'
    assert dct.lookup('frigging') == 'friging'
    assert dct.lookup('singin') == 'singin'
    assert dct.lookup('Singin') == 'Singin'
    assert dct.lookup('SINGIN') == 'SINGIN'
    assert dct.lookup('singing') == 'singing'
    # Unless the respelled -ing word does NOT end in -ing
    assert dct.lookup('yealin') is None
    assert dct.lookup('yealing') == 'yeelin'


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
    assert conv.convert_para('This is a sentence.') == 'Thiss is a sentenss.'
    assert conv.convert_para('Some words in "double" and \'half quotation\' marks.') ==\
        'Sum wurds in "dubl" and \'haf quoataition\' marks.'
    assert conv.convert_para('Some words in typographic “double” and ‘half quotation’ marks.') ==\
        'Sum wurds in typografic “dubl” and ‘haf quoataition’ marks.'

def test_convert_para_spacy_quotes(conv):
    """Test that quote marks are also handled correctly if spaCy is invoked."""
    assert conv.convert_para('Who could object to some words in "double" and \'half quotation\' marks?') ==\
        'Hu cood object tu sum wurds in "dubl" and \'haf quoataition\' marks?'
    assert conv.convert_para('Who could object to some words in typographic “double” and ‘half quotation’ marks?') ==\
        'Hu cood object tu sum wurds in typografic “dubl” and ‘haf quoataition’ marks?'
    assert conv.convert_para('Who could object to some words in typographic ‘‘fake double quotation’’ marks?') ==\
        'Hu cood object tu sum wurds in typografic ‘‘faik dubl quoataition’’ marks?'

def test_convert_para_contractions(conv):
    assert conv.convert_para("Let's hope contractions are handled correctly wheresoe'er they'll occur, don't you think so, O'Connell?") ==\
        'Let’s hoap contractions ar handeld kerectli wairso’air thay’l ocur, doan’t ue think so, O’Conel?'
    assert conv.convert_para('Let’s hope contractions are handled correctly wheresoe’er they’ll occur, don’t you think so, O’Connell?') ==\
        'Let’s hoap contractions ar handeld kerectli wairso’air thay’l ocur, doan’t ue think so, O’Conel?'
    assert conv.convert_para("He's happy to see my boyfriend's sister.") ==\
        'Hi’s hapi tu see my boyfrend’s sister.'
    assert conv.convert_para('He’s happy to see my boyfriend’s sister.') ==\
        'Hi’s hapi tu see my boyfrend’s sister.'

def test_convert_para_nonstandard_contractions(conv):
    """Ensure that nonstandard contractions are accepted as well."""
    assert conv.convert_para("Sue'll do it 'cause she can.") ==\
        "Su’l du it 'caus shi can."
    assert conv.convert_para('Sue’ll do it ’cause she can.') ==\
        'Su’l du it ’caus shi can.'
    assert conv.convert_para('Peter’d know what to do.') ==\
        'Peeter’d noa whot tu du.'
    assert conv.convert_para("What're they going to do?") ==\
        'Whot’r thay going tu du?'
    assert conv.convert_para('Where’ve they put it?') ==\
        'Whair’v thay poot it?'

def test_convert_para_nonstandard_contractions_spacy(conv):
    """Likewise with spaCy."""
    assert conv.convert_para("I object: Sue'll do it 'cause she can.") ==\
        'Y object: Su’l du it ’caus shi can.'
    assert conv.convert_para('I object: Sue’ll do it ’cause she can.') ==\
        'Y object: Su’l du it ’caus shi can.'
    assert conv.convert_para('I estimate: Peter’d know what to do.') ==\
        'Y éstimait: Peeter’d noa whot tu du.'
    assert conv.convert_para("I object: What're they going to do?") ==\
        'Y object: Whot’r thay going tu du?'

def test_convert_para_pos_tagged(conv):
    assert conv.convert_para('I did not object to the object.') == 'Y did not object tu the óbject.'
    assert conv.convert_para('They were too close to the door to close it.') ==\
        'Thay wur tuu cloass tu the dor tu cloas it.'
    assert conv.convert_para('Before I mow the lawn let me place this grain in the mow.') ==\
        'Bifor y mo the laun let mi plaiss thiss grain in the mow.'
    assert conv.convert_para('He thought it was time to present the present.') ==\
        'Hi thaut it wos tym tu prisént the present.'
    # Test disabled since it doesn't work in the latest version (as of Aug. 2022)
    ##assert conv.convert_para('I met an august man last August.') ==\
    ##    'Y met an augúst man last August.'
    assert conv.convert_para('To help with planting, the farmer taught his sow to sow.') ==\
        'Tu help with planting, the farmer taut his sow tu so.'
    assert conv.convert_para('The weather was beginning to affect his affect.') ==\
        'The wether wos bigíning tu afect his áfect.'
    assert conv.convert_para('We must polish the Polish furniture.') ==\
        'Wi must polish the Poalish furnicher.'
    assert conv.convert_para('The dump was so full that it had to refuse more refuse.') ==\
        'The dump wos so fool that it had tu rifues mor réfuess.'
    assert conv.convert_para('I had to subject the subject to a series of tests.') ==\
        'Y had tu subjéct the subject tu a seerees ov tests.'
    assert conv.convert_para('Don’t desert me here in the desert!') ==\
        'Doan’t disurt mi heer in the desert!'
    assert conv.convert_para('The outright prohibition has caused smoking to be banned outright.') ==\
            'The óutryt prohibition has causd smoaking tu bee band outryt.'

def test_convert_para_nt_contractions(conv):
    assert conv.convert_para("Don't you think I won't do it, because I will!") ==\
            'Doan’t ue think y woan’t du it, bicaus y wil!'
    assert conv.convert_para('Don’t you think I won’t do it, because I will!') ==\
            'Doan’t ue think y woan’t du it, bicaus y wil!'
    assert conv.convert_para("You mustn't believe that they can't do such a thing.") ==\
            'Ue musn’t bileev that thay can’t du such a thing.'
    assert conv.convert_para('You mustn’t believe that they can’t do such a thing.') ==\
            'Ue musn’t bileev that thay can’t du such a thing.'

def test_convert_para_initial_nt(conv):
    """Leading "n't", though certainly irregular, shouldn't cause problems.

    Rather, it should be treated as an unknown token and returned as is.
    """
    assert conv.convert_para("N't a good way to open a sentence.") ==\
            "N't a good way tu oapen a sentenss."
    assert conv.convert_para('N’t a good way to open a sentence.') ==\
            'N’t a good way tu oapen a sentenss.'

def test_convert_para_diacritics(conv):
    assert conv.convert_para('Mañana me and my naïve doppelgänger will eat tōfu in the café of an élite hôtel.') ==\
      'Maanyáanaa mi and my naaéev dopelganger wil eet toafu in the cafay ov an ileet hoatél.'


def test_convert_i_case(conv):
    """Test case of converted "I" and contractions such as "I'd", "I'll".

    They should be capitalized at the start, but not in the middle of sentences.
    """
    assert conv.convert_para("I am capitalized at the start of sentences but I'm lower-case in the middle. I am still capitalized at the start.") ==\
            'Y am capitelysd at the start ov sentensses but y’m loer-caiss in the midel. Y am stil capitelysd at the start.'
    assert conv.convert_para('I’d be capitalized at the start of sentences but I’ll be lower-case in the “middle”! I’d still be capitalized at the start.') ==\
            'Y’d bee capitelysd at the start ov sentensses but y’l bee loer-caiss in the “midel”! Y’d stil bee capitelysd at the start.'
    assert conv.convert_para("I've seen that that works but I've not seen whether this works? I've seen it now.") ==\
            'Y’v seen that that wurks but y’v not seen whether thiss wurks? Y’v seen it now.'

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
    """Test that "I" case conversion also works if spaCy is invoked."""
    assert conv.convert_para("I estimate that I am capitalized at the start of sentences but that I'm lower-case in the middle. I am still capitalized at the start.") ==\
            'Y éstimait that y am capitelysd at the start ov sentensses but that y’m loer-caiss in the midel. Y am stil capitelysd at the start.'
    assert conv.convert_para('Opening quote marks don’t hurt, I estimate. “I am still capitalized.”') ==\
            'Oapening quoat marks doan’t hurt, y éstimait. “Y am stil capitelysd.”'
    assert conv.convert_para('"Closing trailing quote marks don\'t hurt, I estimate?" I\'m still capitalized.') ==\
            '"Cloasing trailing quoat marks doan’t hurt, y éstimait?" Y’m stil capitelysd.'
    assert conv.convert_para('Closing leading quote marks don’t hurt, ‘I’d estimate’! I am still capitalized.') ==\
            'Cloasing leeding quoat marks doan’t hurt, ‘y’d éstimait’! Y am stil capitelysd.'
    assert conv.convert_para('After a colon: I should not be capitalized, I’d estimate.') ==\
            'After a coalen: y shood not bee capitelysd, y’d éstimait.'
    assert conv.convert_para('I said: “I estimate I’ll be capitalized now.”') ==\
            'Y sed: “Y éstimait y’l bee capitelysd now.”'
    assert conv.convert_para('I estimate that around 11 Feb. 2019 I wrote a test.') ==\
            'Y éstimait that eround 11 Feb. 2019 y roat a test.'
    assert conv.convert_para('Even after a fraction such as 3.5 I should still be lower-case, I estimate.') ==\
            'Eeven after a fraction such as 3.5 y shood stil bee loer-caiss, y éstimait.'

def test_convert_i_vs_roman_i(conv):
    """Test that the pronoun 'I' and the roman number 'I' are distinguished."""
    assert conv.convert_para('I survived World War I, but many others didn’t.') ==\
            'Y servyvd Wurld Wor I, but meni uthers didn’t.'


def test_convert_hyphenated_prefixes(conv):
    assert conv.convert_para('I re-sent the file to you last night.') ==\
        'Y ri-sent the fyl tu ue last nyt.'
    # Test that it works with spaCy too
    assert conv.convert_para('I estimate that I have to re-press the shirt.') ==\
        'Y éstimait that y hav tu ri-press the shurt.'
    # Also if capitalized
    assert conv.convert_para('The Re-Creation of a Lost Artwork') ==\
        'The Ri-Criaition ov a Lost Artwurk'
    # Nothing should happen in other cases
    assert conv.convert_para('I talked with her re the case.') ==\
        'Y taukd with hur ree the caiss.'
    assert conv.convert_para('Re: Your mail') ==\
        'Ree: Uer mail'
    # Also test some other prefix
    assert conv.convert_para('She de-emphasized our differences.') ==\
            'Shi di-émfecysd our diferensses.'
    assert conv.convert_para('The Paleo-Americans arrived in the Americas a long, long time ago.') ==\
        'The Palio-Amerricans eryvd in the Amerricas a long, long tym ago.'

def test_convert_other_hyphens(conv):
    """Test that hyphens between words don't cause problems."""
    # Hyphens between independent words
    assert conv.convert_para('We need a state-of-the-art solution.') ==\
        'Wi need a stait-ov-the-art soluution.'
    # Not a prefix, since there is whitespace after the hyphen
    assert conv.convert_para('I’m contacting you re- you might have guessed it -your mail.') ==\
        'Y’m contacting ue ree- ue myt hav gessd it -uer mail.'
    # Likewise if another hyphen follows
    assert conv.convert_para('I’m contacting you re--you might have guessed it--your mail.') ==\
        'Y’m contacting ue ree--ue myt hav gessd it--uer mail.'

def test_convert_urls(conv):
    """Test that URLs are not converted."""
    assert conv.convert_para('This is a link: www.lytspel.org/overview, hence it should not be converted.') ==\
        'Thiss is a link: www.lytspel.org/overview, henss it shood not bee convurted.'
    assert conv.convert_para('This is a link: https://www.lytspel.org/idea which is not converted.') ==\
        'Thiss is a link: https://www.lytspel.org/idea which is not convurted.'
    assert conv.convert_para('This is a link: <https://www.lytspel.org/limitations>, hence it should not be converted.') ==\
        'Thiss is a link: <https://www.lytspel.org/limitations>, henss it shood not bee convurted.'
    assert conv.convert_para('See the subsequent link (https://www.lytspel.org/rules) which is not converted.') ==\
        'See the subsiquent link (https://www.lytspel.org/rules) which is not convurted.'
    assert conv.convert_para('See the subsequent link [https://www.lytspel.org/overview], which is not converted.') ==\
        'See the subsiquent link [https://www.lytspel.org/overview], which is not convurted.'

def test_convert_urls_spacy(conv):
    """Ditto with spaCy."""
    assert conv.convert_para('This, I object, is a link: www.lytspel.org/overview, hence it should not be converted.') ==\
        'Thiss, y object, is a link: www.lytspel.org/overview, henss it shood not bee convurted.'
    assert conv.convert_para('This, I object, is a link: https://www.lytspel.org/idea which is not converted.') ==\
        'Thiss, y object, is a link: https://www.lytspel.org/idea which is not convurted.'
    assert conv.convert_para('This, I object, is a link: <https://www.lytspel.org/limitations>, hence it should not be converted.') ==\
        'Thiss, y object, is a link: <https://www.lytspel.org/limitations>, henss it shood not bee convurted.'
    assert conv.convert_para('I object: See the subsequent link (https://www.lytspel.org/rules) which is not converted.') ==\
        'Y object: See the subsiquent link (https://www.lytspel.org/rules) which is not convurted.'
    assert conv.convert_para('I object: See the subsequent link [https://www.lytspel.org/overview], which is not converted.') ==\
        'Y object: See the subsiquent link [https://www.lytspel.org/overview], which is not convurted.'

def test_convert_emails(conv):
    """Test that email addressed are not converted."""
    assert conv.convert_para('This is an email: christian@example.org, hence it should not be converted.') ==\
        'Thiss is an eemail: christian@example.org, henss it shood not bee convurted.'
    assert conv.convert_para('This is an email: mailto:christian@example.org which is not converted.') ==\
        'Thiss is an eemail: mailto:christian@example.org which is not convurted.'
    assert conv.convert_para('This is an email: <christian@example.org>, hence it should not be converted.') ==\
        'Thiss is an eemail: <christian@example.org>, henss it shood not bee convurted.'
    assert conv.convert_para('See the subsequent email (christian@example.org) which is not converted.') ==\
        'See the subsiquent eemail (christian@example.org) which is not convurted.'

def test_convert_emails_spacy(conv):
    """Ditto with spaCy."""
    assert conv.convert_para('This, I object, is an email: christian@example.org, hence it should not be converted.') ==\
        'Thiss, y object, is an eemail: christian@example.org, henss it shood not bee convurted.'
    assert conv.convert_para('This, I object, is an email: mailto:christian@example.org which is not converted.') ==\
        'Thiss, y object, is an eemail: mailto:christian@example.org which is not convurted.'
    assert conv.convert_para('This, I object, is an email: <christian@example.org>, hence it should not be converted.') ==\
        'Thiss, y object, is an eemail: <christian@example.org>, henss it shood not bee convurted.'
    assert conv.convert_para('I object: See the subsequent email (christian@example.org) which is not converted.') ==\
        'Y object: See the subsiquent eemail (christian@example.org) which is not convurted.'

def test_is_word_simple(is_word):
    assert is_word('a')
    assert is_word('This')
    assert is_word('sentence')
    assert is_word("Let's")
    assert not is_word('')
    assert not is_word(' ')
    assert not is_word('.')
    assert not is_word('; ')
    assert not is_word("'")
    assert not is_word('’')
    assert not is_word('“')

def test_is_word_apostrophe(is_word):
    """Words starting with an apostrophe are recognized as such.

    SpaCy's tokenizer produces such words.
    """
    assert is_word("'d")
    assert is_word("'re")
    assert is_word('’d"')
    assert is_word('’re')

def test_is_word_diacritics(is_word):
    assert is_word('œuvre')
    assert is_word('élite')
    assert is_word('épée')
    assert is_word('doppelgänger')
    assert is_word('Œuvre')
    assert is_word('Élite')
    assert is_word('ÉPÉE')


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
