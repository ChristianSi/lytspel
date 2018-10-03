// Background script for loading the Lytspel dictionary and converting words from tradspell
// to Lytspel.

'use strict';  // strict mode

// --- Global ---
const dict = {}; // will be filled later


// --- Entry point ---
loadDict();
browser.runtime.onMessage.addListener(convertWordset);

// Trigger loading the dictionary.
function loadDict() {
  const dictFileName = browser.extension.getURL('data/lytspel-dict.csv');
  const xmlhttp = new XMLHttpRequest();
  xmlhttp.responseType = 'text';
  xmlhttp.open('GET', dictFileName);
  xmlhttp.onreadystatechange = function () {
    if(xmlhttp.readyState === 4) {
      if(xmlhttp.status === 200 || xmlhttp.status == 0) {
        fillDict(xmlhttp.responseText);
      }
    }
  }
  xmlhttp.send();
}

// Convert all known words in wordset to Lytspel, returning a mapping from tradspell to
// Lytspel. Unknown words will be omitted from the returned mapping.
function convertWordset(wordset, sender, sendResponse) {
  const result = {};

  for (const word of wordset) {
    const conv = dict[word];
    // TODO Handle case, typographical apostrophes, genitive etc.
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
    console.log(fields[0] + ' -> ' + fields[3]);
    // TODO Actually add contents (Traditional,POS,Redirect,Lytspel), skipping last
    // empty entry
  }
}
