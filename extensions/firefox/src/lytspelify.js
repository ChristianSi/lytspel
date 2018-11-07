'use strict';  // strict mode


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
function collectWordsFromChildNodes(node, wordset) {
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
// an ASCII letter and may contain apostrophes.
function tokenizeText(text) {
  const result = text.split(/([a-zA-Z](?:['’]?[a-zA-Z])*)/);
  // Remove first and/or last element if they are empty
  if (result[0] === '') { result.shift(); }
  if (result[result.length - 1] === '') { result.pop(); }
  return result;
}

// Check if a token is a word. Words must start with an ASCII letter.
function isWord(token) {
  return /^[a-zA-Z]/.test(token);
}

// Iterate the child nodes of an node. Element nodes are traversed recursively, while the
// content of text nodes is converted to Lytspel.
function convertChildNodes(node, words) {
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

// Tokenize `text` and convert all word tokens to Lytspel.
function convertText(text, words) {
  let tokens = tokenizeText(text);
  tokens = tokens.map(x => convertToken(x, words) );
  return tokens.join('');
}

// Convert `token` to Lytspel. Non-word tokens and tokens not listed in the `words`
// object will be returned unchanged.
function convertToken(token, words) {
  if (!isWord(token)) return token;
  return words[token] || token;
}

// --- Test code and helpers ---

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
  // using join in tests for readability
  assert.equal(tokenizeText('This is a sentence.').join('|'), 'This| |is| |a| |sentence|.');
  assert.equal(tokenizeText('A sentence without final punctuation').join('|'),
    'A| |sentence| |without| |final| |punctuation');
  assert.equal(tokenizeText('Sentence with-some inner, punctuation;this should - not! — cause problems?Let    us\thope so!').join('|'),
    'Sentence| |with|-|some| |inner|, |punctuation|;|this| |should| - |not|! — |cause| |problems|?|Let|    |us|\t|hope| |so|!');
  assert.equal(tokenizeText('Some words in "double" and \'half quotation\' marks.').join('|'),
    'Some| |words| |in| "|double|" |and| \'|half| |quotation|\' |marks|.');
  assert.equal(tokenizeText('Some words in typographic “double” and ‘half quotation’ marks.').join('|'),
    'Some| |words| |in| |typographic| “|double|” |and| ‘|half| |quotation|’ |marks|.');
  assert.equal(tokenizeText("Let's hope contractions are handled correctly wheresoe'er they'll occur, don't you think so, O'Connell?").join('|'),
    "Let's| |hope| |contractions| |are| |handled| |correctly| |wheresoe'er| |they'll| |occur|, |don't| |you| |think| |so|, |O'Connell|?");
  assert.equal(tokenizeText('Let’s hope contractions are handled correctly wheresoe’er they’ll occur, don’t you think so, O’Connell?').join('|'),
    'Let’s| |hope| |contractions| |are| |handled| |correctly| |wheresoe’er| |they’ll| |occur|, |don’t| |you| |think| |so|, |O’Connell|?');
  assert.equal(tokenizeText("He's happy to see my boyfriend's sister.").join('|'),
    "He's| |happy| |to| |see| |my| |boyfriend's| |sister|.");
  assert.equal(tokenizeText('He’s happy to see my boyfriend’s sister.').join('|'),
    'He’s| |happy| |to| |see| |my| |boyfriend’s| |sister|.');
  // assert.equal(tokenizeText('').join('|'), '');
  console.log('Test passed');
}
