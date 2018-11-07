// Background script for loading the Lytspel dictionary and converting words from tradspell
// to Lytspel.

'use strict';  // strict mode

// --- Globals ---
const DICT_FILE = 'data/lytspel-dict.csv';
const dict = new Map();  // dicts will be filled later
const mixedDict = new Map();


// --- Entry point ---
loadDict();
if (!testMode()) {
  browser.runtime.onMessage.addListener(convertWordset);
}

// Trigger loading the dictionary.
function loadDict() {
  if (testMode()) {
    let fs = require('fs');
    fs.readFile('../' + DICT_FILE, 'utf8', (err, data) => {
      if (err) throw err;
      fillDicts(data);
      selfTest();
    });
    return;
  }

  const xmlhttp = new XMLHttpRequest();
  xmlhttp.responseType = 'text';
  xmlhttp.open('GET', browser.extension.getURL(DICT_FILE));

  xmlhttp.onreadystatechange = () => {
    if (xmlhttp.readyState === 4 && (xmlhttp.status === 200 || xmlhttp.status == 0)) {
      fillDicts(xmlhttp.responseText);
    }
  }
  xmlhttp.send();
}

// Convert all known words in wordset to Lytspel, returning a mapping from tradspell to
// Lytspel. Unknown words will be omitted from the returned mapping.
function convertWordset(wordset, sender, sendResponse) {
  const result = {};

  for (const word of wordset) {
    const conv = lookup(word);
    // TODO Also handle genitives etc.
    // Add POS tagging and handle tags both when filling the dict (fillDicts) and here.
    if (conv !== undefined) result[word] = conv;
  }

  sendResponse(result);
}


// --- Callbacks ---

// Fill the dictionary globals from the contents of the CSV file defining the Lytspel dictionary.
function fillDicts(contents) {
  const lines = contents.split(/\r?\n/);
  lines.shift();  // skip header line

  for (const line of lines) {
    const fields = line.split(',');
    const tradspell = fields[0];
    const lytspel = fields[3];

    if (tradspell && lytspel) {
      dict.set(tradspell.toLowerCase(), lytspel.toLowerCase());
      if (isMixedCase(lytspel)) {
        mixedDict.set(tradspell, lytspel);
      }
    }
    // TODO Add support for POS tag (1) and Redirects (2)
  }
}


// --- Helpers ---

// A word is assumed to be MiXed case if it starts with an upper-case letter and if it
// contains at least one other upper-case and one lower-case letter.
function isMixedCase(word) {
  return /^[A-Z].*[a-z]/.test(word) && /.[A-Z]/.test(word);
}

// Lookup a word in the dictionary and returned its converted form, restoring the case
// (lower, Capitalized, or ALL_CAPS). MixedCase is also restored provided that *both* the input
// word and the dictionary entry use this case form (e.g. JavaScript -> JaavaScript).
function lookup(word) {
  word = word.split('’').join("'"); // replace typographic by normal apostrophe
  let genitiveS = '';

  if (/'s$/i.test(word)) { 
    // Strip final 's (genitive or contraction) and remember for later (handling case)
    genitiveS = word.slice(-2);
    word = word.slice(0, -2);
  }

  let result = dict.get(word.toLowerCase());
  if (result == undefined) return result;

  if (/^[A-Z']+$/.test(word)) {
    result = result.toUpperCase();  // ALL_CAPS
  } else if (/^[A-Z]/.test(word)) {
    if (mixedDict.has(word)) {
      result = mixedDict.get(word); // MixedCase
    } else {
      result = result[0].toUpperCase() + result.slice(1);  // Capitalized
    }
  }

  if (genitiveS) {
    result += genitiveS;
  }

  result = result.split("'").join('’'); // replace normal by typographic apostrophe
  return result;
}


// --- Test code and helpers ---

// Check whether this code is running in test mode (without a browser object).
function testMode() {
  return typeof browser === 'undefined';
}

// Run some tests.
function selfTest() {
  console.log('Running self-test...');
  let assert = require('assert');
  assert.equal(lookup('blackbird'), 'blakburd');
  assert.equal(lookup('earthward'), 'urthwerd');
  assert.equal(lookup('shadows'), 'shadoas');
  assert.equal(lookup('sharp'), 'sharp');
  assert.equal(lookup('technology'), 'tec’noleji');
  assert.equal(lookup('tahiti'), 'ta’heeti');   // capitalized in dict
  assert.equal(lookup('notaword'), undefined);
  // Case should be restored
  assert.equal(lookup('Score'), 'Scoar');
  assert.equal(lookup('NETWORK'), 'NETWURK');
  assert.equal(lookup('POLITICAL'), 'PO’LITICL');
  assert.equal(lookup('JavaScript'), 'JaavaScript');
  assert.equal(lookup('McCain'), 'McCain');
  assert.equal(lookup('MeV'), 'MeV');
  assert.equal(lookup('ODed'), 'ODed');
  assert.equal(lookup("O'Donnell"), 'O’Donel');
  assert.equal(lookup('PCs'), 'PCs');
  assert.equal(lookup('PowerPC'), 'PowerPC');
  // Both normal and typographic apostrophes should be accepted
  assert.equal(lookup("I'm"), 'Y’m');
  assert.equal(lookup('I’m'), 'Y’m');
  assert.equal(lookup("it'll"), 'it’l');
  assert.equal(lookup('it’ll'), 'it’l');
  assert.equal(lookup("O'Connell"), 'O’Conel');
  assert.equal(lookup('O’Connell'), 'O’Conel');
  // 's (genitive or contraction) is handled correctly (normal and typographic forms)
  assert.equal(lookup("He's"), 'Hi’s');
  assert.equal(lookup("boyfriend's"), 'boyfrend’s');
  assert.equal(lookup('He’s'), 'Hi’s');
  assert.equal(lookup('boyfriend’s'), 'boyfrend’s');
  // Upper case words too
  assert.equal(lookup("HE's"), 'HI’s');
  assert.equal(lookup('He’s'), 'Hi’s');

  //assert.equal(lookup(''), '');
  // TODO Handle and test here and in other file:
  // * Diacritical letters
  // * Heuristic for language detection: don't convertText unless at least 40 % of its
  //   words are known.
  // * Skip script+style tags; also convert alt+title attributes; ensure that no problems
  //   remain on e.g.spiegel.de, focus.de
  // * POS tags -- test using https://en.wikipedia.org/wiki/Heteronym_(linguistics)#Examples
  // * redirects
  // * Special case: I and I'm should only remain capitalized at the beginning of sentences
  // * Modify Makefile to also update extensions/firefox/data whenever lytspel-dict.csv has changed
  // * Recognize URLs in text and treat as non-words (sample: IRC logs)
  // * Use ESLint (node package) to check the coding style
  // * Add button to turn conversion off and on
  // * If called in test mode with argument, convert txt/html/epub file, storing the original
  //   as FILE-orig.EXT
  // * Also rewrite parts of a page loaded later (e.g. due to scrolling; sample: Youtube
  //   comments); possibly use a custom data-\* attribute; can be read and written as
  //   elem.dataset.NAME, e.g. elem.dataset.lytspel = conv|seen
  // * Add form that allows the conversion of local txt/html/epub files (different interface
  //   for same functionality)
  // * Add a dialog for converting words and sentences to Lytspel
  // * Package extension (see NOTES), tell Matthias, and upload to plugin directory
  console.log('Self-test passed');
}
