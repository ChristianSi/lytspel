'use strict';  // strict mode


// --- Globals ---
const LOWERCASE_LETTER_PAT =
  '[a-záàăâǎåäãąāảæćĉčċçďđéèĕêěëẽęēẻǵğĝǧġḡĥȟḧħíìĭîǐïĩįīĵǰḱǩĺľłḹḿńǹňñóòŏôǒöõøǫōỏœṕŕřśŝšşßťẗúùŭûǔůüũųūủṽẃẁẘẅẍýỳŷẙÿỹȳỷźẑž]';
const TOKEN_RE = new RegExp(
  '(' + LOWERCASE_LETTER_PAT + "(?:['’]?" + LOWERCASE_LETTER_PAT +')*)',
  'i');
const STARTS_WITH_LETTER_RE = new RegExp('^' + LOWERCASE_LETTER_PAT, 'i');
const ATTRIBS_TO_CONVERT = ['alt', 'title'];

// --- Entry point ---
if (!testMode()) {
  triggerLytspelConversion(document.body, document.title);
}

// Trigger the Lytspel conversion of the current document. Collect all words in the document
// body and title and send them to the background script for conversion, setting a callback
// that will do the actual conversion once the background script replies.
function triggerLytspelConversion() {
  let wordset = new Set();
  collectWordsFromChildNodes(document.body, wordset);
  collectWords(document.title, wordset);
  // Send wordset to background script and register callback
  browser.runtime.sendMessage(wordset).then(convertToLytspel);
}


// --- Primary helpers and callbacks ---

// Iterate the child nodes of an node. Element nodes are traversed recursively, while the
// content of text nodes is tokenize and all word tokens are added to `wordset`.
// Textual content of any ATTRIBS_TO_CONVERT is collected as well.
function collectWordsFromChildNodes(node, wordset) {
  // Collect from relevant attributes
  for (const attrib of ATTRIBS_TO_CONVERT) {
    collectWords(node.getAttribute(attrib), wordset);
  }

  // Collect from child nodes
  for (const child of node.childNodes) {
    switch(child.nodeType) {
      case Node.ELEMENT_NODE:
        collectWordsFromChildNodes(child, wordset);
        break;
      case Node.TEXT_NODE:
        collectWords(child.nodeValue, wordset);
        break;
    }
  }
}

// Tokenize `text` and add all word tokens to `wordset`.
function collectWords(text, wordset) {
  if (!text) return;  // nothing to do
  const tokens = tokenizeText(text);

  for (const token of tokens) {
    if (isWord(token)) wordset.add(token);
  }
}

// Callback that converts the current document (including its title) to Lytspel.
function convertToLytspel(words) {
  convertChildNodes(document.body, words);
  document.title = convertText(document.title, words);
}


// --- Secondary helpers ---

// Tokenize a string, returning an array of words and puncuation. Words must start and end with
// a letter and may contain apostrophes.
function tokenizeText(text) {
  const result = text.split(TOKEN_RE);
  // Remove first and/or last element if they are empty
  if (result[0] === '') result.shift();
  if (result[result.length - 1] === '') result.pop();
  return result;
}

// Check if a token is a word. Words must start with a letter.
function isWord(token) {
  return STARTS_WITH_LETTER_RE.test(token);
}

// Iterate the child nodes of an node. Element nodes are traversed recursively, while the
// content of text nodes is converted to Lytspel. Textual content of any ATTRIBS_TO_CONVERT
// is converted as well.
function convertChildNodes(node, words) {
  // Convert relevant attributes
  for (const attrib of ATTRIBS_TO_CONVERT) {
    if (! node.hasAttribute(attrib)) continue;  // Nothing to do
    node.setAttribute(attrib, convertText(node.getAttribute(attrib), words));
  }

  // Convert child nodes
  for (const child of node.childNodes) {
    switch(child.nodeType) {
      case Node.ELEMENT_NODE:
        convertChildNodes(child, words);
        break;
      case Node.TEXT_NODE:
        child.nodeValue = convertText(child.nodeValue, words);
        break;
    }
  }
}

// Tokenize `text` and convert all word tokens to Lytspel. To prevent accidental
// modification to specific words in foreign-language texts, `text` will only be
// converted if at least half of its words are listed in the dictionary
// (otherwise it will be returned unchanged).
function convertText(text, words) {
  let tokens = tokenizeText(text);
  let knownWords = 0;
  let unknownWords = 0;

  for (let i = 0; i < tokens.length; i++) {
    const token = tokens[i];
    if (!isWord(token)) continue;  // not a word
    const conv = words[token];

    if (conv) {
      ++knownWords;
      tokens[i] = conv;
    } else {
      ++unknownWords;
    }
  }

  if (unknownWords <= knownWords) {
    // at least half of the words are known
    return tokens.join('');
  } else {
    return text; // return text unchanged
  }
}


// --- Test code ---

if (testMode()) {
  selfTest();
}

// Check whether this code is running in test mode (without a document object).
function testMode() {
  return typeof document === 'undefined';
}

// Run some tests.
function selfTest() {
  console.log('Running self-test...');
  let assert = require('assert');
  testTokenizeSimple(assert);
  testTokenizeContractions(assert);
  testTokenizeDiacritics(assert);
  testIsWordSimple(assert);
  testIsWordDiacritics(assert);
}

// Note: join is used in tokenization tests for readability.
function testTokenizeSimple(assert) {
  assert.equal(tokenizeText('This is a sentence.').join('|'), 'This| |is| |a| |sentence|.');
  assert.equal(tokenizeText('A sentence without final punctuation').join('|'),
    'A| |sentence| |without| |final| |punctuation');
  assert.equal(tokenizeText('Sentence with-some inner, punctuation;this should - not! — cause problems?Let    us\thope so!').join('|'),
    'Sentence| |with|-|some| |inner|, |punctuation|;|this| |should| - |not|! — |cause| |problems|?|Let|    |us|\t|hope| |so|!');
  assert.equal(tokenizeText('Some words in "double" and \'half quotation\' marks.').join('|'),
    'Some| |words| |in| "|double|" |and| \'|half| |quotation|\' |marks|.');
  assert.equal(tokenizeText('Some words in typographic “double” and ‘half quotation’ marks.').join('|'),
    'Some| |words| |in| |typographic| “|double|” |and| ‘|half| |quotation|’ |marks|.');
}

function testTokenizeContractions(assert) {
  assert.equal(tokenizeText("Let's hope contractions are handled correctly wheresoe'er they'll occur, don't you think so, O'Connell?").join('|'),
    "Let's| |hope| |contractions| |are| |handled| |correctly| |wheresoe'er| |they'll| |occur|, |don't| |you| |think| |so|, |O'Connell|?");
  assert.equal(tokenizeText('Let’s hope contractions are handled correctly wheresoe’er they’ll occur, don’t you think so, O’Connell?').join('|'),
    'Let’s| |hope| |contractions| |are| |handled| |correctly| |wheresoe’er| |they’ll| |occur|, |don’t| |you| |think| |so|, |O’Connell|?');
  assert.equal(tokenizeText("He's happy to see my boyfriend's sister.").join('|'),
    "He's| |happy| |to| |see| |my| |boyfriend's| |sister|.");
  assert.equal(tokenizeText('He’s happy to see my boyfriend’s sister.').join('|'),
    'He’s| |happy| |to| |see| |my| |boyfriend’s| |sister|.');
  console.log('Test passed');
}

function testTokenizeDiacritics(assert) {
  assert.equal(tokenizeText('Mañana me and my naïve doppelgänger will eat tōfu in the café of an élite hôtel.').join('|'),
    'Mañana| |me| |and| |my| |naïve| |doppelgänger| |will| |eat| |tōfu| |in| |the| |café| |of| |an| |élite| |hôtel|.');
  assert.equal(tokenizeText('MAÑANA me and my naïve doppelGÄNGER will eat tōfu in the CAFÉ of an Élite Hôtel.').join('|'),
    'MAÑANA| |me| |and| |my| |naïve| |doppelGÄNGER| |will| |eat| |tōfu| |in| |the| |CAFÉ| |of| |an| |Élite| |Hôtel|.');
}

function testIsWordSimple(assert) {
  assert.ok(isWord('a'));
  assert.ok(isWord('This'));
  assert.ok(isWord('sentence'));
  assert.ok(isWord("Let's"));
  assert.equal(isWord(''), false);
  assert.equal(isWord(' '), false);
  assert.equal(isWord('.'), false);
  assert.equal(isWord('; '), false);
  assert.equal(isWord("'"), false);
  assert.equal(isWord('’'), false);
  assert.equal(isWord('“'), false);
}

function testIsWordDiacritics(assert) {
  assert.ok(isWord('œuvre'));
  assert.ok(isWord('élite'));
  assert.ok(isWord('épée'));
  assert.ok(isWord('doppelgänger'));
  assert.ok(isWord('Œuvre'));
  assert.ok(isWord('Élite'));
  assert.ok(isWord('ÉPÉE'));
}

// assert.equal(tokenizeText('').join('|'), '');
