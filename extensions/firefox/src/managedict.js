// Background script for loading the Lytspel dictionary and converting words from tradspell
// to Lytspel.

'use strict';  // strict mode

// --- Global ---
const dict = {}; // will be filled later


// --- Entry point ---
loadDict();
if (!testMode()) { // TODO
  browser.runtime.onMessage.addListener(convertWordset);
} else {
  console.log('Testing...');
}

// Trigger loading the dictionary.
function loadDict() {
  if (testMode()) { // TODO
    console.log('Skipping dict creation');
    return;
  }
  const dictFileName = browser.extension.getURL('data/lytspel-dict.csv');
  const xmlhttp = new XMLHttpRequest();
  xmlhttp.responseType = 'text';
  xmlhttp.open('GET', dictFileName);
  xmlhttp.onreadystatechange = function () {
    if (xmlhttp.readyState === 4 && (xmlhttp.status === 200 || xmlhttp.status == 0)) {
      fillDict(xmlhttp.responseText);
    }
  }
  xmlhttp.send();
}

// Convert all known words in wordset to Lytspel, returning a mapping from tradspell to
// Lytspel. Unknown words will be omitted from the returned mapping.
function convertWordset(wordset, sender, sendResponse) {
  const result = {};

  for (const word of wordset) {
    const conv = dict[word]; // lookup(word);
    // TODO Test using test dir, preferably via
    // https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Getting_started_with_web-ext
    // TODO Handle typographical apostrophes: recognize both forms input; always output
    // typographic variant unless the source file is ASCII-only
    // Also handle genitives etc.
    // Add POS tagging and handle tags both when filling the dict (fillDict) and here.
    if (conv !== undefined) result[word] = conv;
  }

  sendResponse(result);
}


// --- Callbacks ---

// Fill the 'dict' global from the contents of the CSV file defining the Lytspel dictionary.
function fillDict(contents) {
  const lines = contents.split(/\r?\n/);
  lines.shift();  // skip header line

  for (const line of lines) {
    const fields = line.split(',');
    const tradspell = fields[0];
    const lytspel = fields[3];
    if (tradspell && lytspel) {
      dict[tradspell.toLowerCase()] = lytspel.toLowerCase();
    }
    // TODO Add support for POS tag (1) and Redirects (2)
  }
}


// --- Helpers ---

// Lookup a word in the dictionary and returned its converted form, restoring the case
// (lower, Capitalized, or ALL_CAPS). MiXed case is also restored provided that *both* the input
// word and the dictionary entry use this case form (e.g. JavaScript -> JaavaScript).
function lookup(word) {
  const result = dict[word.toLowerCase()];
  if (result == undefined) return result;
  // TODO Find out why this does not work and how to print logs
  if (/^[A-Z']+$/.test(word)) {
    result = result.toUpperCase();  // ALL_CAPS
  } else if (/^[A-Z]/.test(word)) {
    result = result[0].toUpperCase() + result.slice(1);  // Capitalized
  }
  // TODO Add test case for ALL_CAPS
  // TODO Output MiXed if both input and lytspelling are mixed
  // TODO Also add and use a cased entry if the lytspelling is mixed case (contains
  // one or more upper-case chars after the first one, e.g. McCain, O'Donel, JaavaScript,
  // MasterCard, MeV, NuetreSweet, ODed, OfissMax, PCs, PlayStaition, PowerPC
 return result;
}


// --- Test code and helpers ---

// Check whether this code is running in test mode (without a browser object).
function testMode() {
  return typeof browser === 'undefined';
}
