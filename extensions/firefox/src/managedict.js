// Background script for loading the Lytspel dictionary and converting words from tradspell
// to Lytspel.

'use strict';  // strict mode


// --- Globals ---

// TODO Instead load actual dictionary
//// "web_accessible_resources": [
//  "images/my-image.png"
//]

//browser.extension.getURL("images/my-image.png");  // returns the filename

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
browser.runtime.onMessage.addListener(convertWordset);

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
