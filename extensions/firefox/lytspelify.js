'use strict';  // strict mode


// TODO Instead load actual dictionary using a background script
let dict = {
  a: "a",
  about: "a'bout",
  after: "after",
  all: "all",
  also: "olso",
  an: "an",
  and: "and",
  any: "eni",
  are: "ar",
  as: "as",
  at: "at",
  back: "bak",
  be: "bee",
  because: "bi'cus",
  been: "been",
  before: "bi'foar",
  being: "beeing",
  between: "bi'tween",
  but: "but",
  by: "by",
  can: "can",
  come: "cum",
  could: "cood",
  did: "did",
  do: "du",
  down: "doun",
  even: "eeven",
  first: "furst",
  for: "for",
  from: "from",
  get: "get",
  go: "go",
  got: "got",
  had: "had",
  has: "has",
  have: "hav",
  he: "hi",
  her: "hur",
  him: "him",
  his: "his",
  how: "how",
  if: "if",
  in: "in",
  into: "intu",
  is: "is",
  it: "it",
  its: "its",
  i: "y",
  just: "just",
  know: "noa",
  like: "lyk",
  look: "look",
  made: "maid",
  make: "maik",
  many: "meni",
  may: "may",
  me: "mi",
  more: "moar",
  most: "moast",
  much: "much",
  my: "my",
  new: "nue",
  no: "no",
  not: "not",
  now: "now",
  of: "ov",
  one: "wun",
  only: "oanli",
  on: "on",
  or: "or",
  other: "udher",
  our: "our",
  out: "out",
  over: "oaver",
  people: "peepl",
  right: "ryt",
  said: "sed",
  see: "see",
  she: "shi",
  should: "shood",
  some: "sum",
  so: "so",
  such: "such",
  than: "dhan",
  that: "dhat",
  the: "dhe",
  their: "dhair",
  them: "dhem",
  then: "dhen",
  there: "dhair",
  these: "dhees",
  they: "dhay",
  think: "think",
  this: "dhiss",
  those: "dhoas",
  time: "tym",
  to: "to",
  two: "tu",
  up: "up",
  very: "verri",
  was: "wos",
  way: "way",
  well: "wel",
  were: "wur",
  we: "wi",
  what: "wut",
  when: "wen",
  where: "wair",
  which: "wich",
  who: "hu",
  will: "wil",
  with: "widh",
  work: "wurk",
  would: "wood",
  years: "yeers",
  you: "iu",
  your: "iur",
};

// --- Entry point ---
// TODO Maybe test if document exists to allow using this script in tests:
// if (typeof document !== 'undefined')
lytspelifyChildNodes(document.body);
document.title = lytspelifyText(document.title);

// Iterate the child nodes of an node. Element nodes are traversed recursively,
// while the content of text nodes is converted to Lytspel.
function lytspelifyChildNodes(node) {
  const childNodes = node.childNodes;

  for (let i = 0; i < childNodes.length; ++i) {
    const child = childNodes[i];

    switch(child.nodeType) {
      case Node.ELEMENT_NODE:
        lytspelifyChildNodes(child);
        break;

      case Node.TEXT_NODE:
        child.nodeValue = lytspelifyText(child.nodeValue);
        break;
    }
  }
}

// Convert text to Lytspel.
function lytspelifyText(text) {
  let tokens = text.split(/([a-zA-Z']+)/);
  // TODO Actually lytspelify the content (initially only for the 100
  // most frequent sample words)
  // TODO Later also handle typographic apostrophe and non-ASCII letters correctly
  // (the latter should be converted for the ASCII equivalents for Lytspel, but
  // left unchanged if the word is unknown)
  // Handling POS tagging and restore case properly
  // (lower, upper, capitalized, mixed)
  tokens = tokens.map(lytspelifyToken);
  return tokens.join('');
}

// Convert a word or other token to Lytspel. Non-words (whitespace and punctuation
// between words) are returned unchanged; unknown words likewise.
function lytspelifyToken(token) {
  if (! /^[a-zA-Z]/.test(token)) return token;  // Not a word
  return dict[token] || token;
}


// --- Primary helpers ---

// TODO Test using Mocha or so
// TODO Use ESLint (node package) to check the coding style
