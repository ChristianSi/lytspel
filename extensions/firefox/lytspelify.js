'use strict';  // strict mode


// --- Entry point ---
// TODO Maybe test if document exists to allow using this script in tests:
// if (typeof document !== 'undefined')
triggerLytspelConversion(document.body, document.title);

// TODO Test whether it works

// Trigger the Lytspel conversion of the current document. Collect all words in the document
// body and title and send them to the background script for conversion, setting a callback
// that will do the actual conversion once the background script replies.
function triggerLytspelConversion() {
  const wordset = new Set();
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
        collectWordsFromChildNodes(child);
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

// Callback that convers the current document (including its title) to Lytspel.
function convertToLytspel(words) {
  convertChildNodes(document.body, words);
  document.title = convertText(document.title, words);
}

// --- Secondary helpers ---

// Tokenize a string, returning an array of words and puncuation. Words must start and end with
// an ASCII letter and may contain apostrophes.
function tokenizeText(text) {
  return text.split(/([a-zA-Z](?:'?[a-zA-Z])*)/);
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
        convertChildNodes(child);
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
  tokens = tokens.map(x => { convertToken(x, words) } );
  return tokens.join('');
}

// Convert `token` to Lytspel. Non-word tokens and tokens not listed in the `words`
// object will be returned unchanged.
function convertToken(token, words) {
  if (!isWord(token)) return token;
  return words[token] || token;
}

// TODO Test using Mocha or so
// TODO Use ESLint (node package) to check the coding style
