# Files

## Files in Root Directory

Some of the files are used to test and build the Python package:

* **comment-dict.txt**: a line-separated list of words that may be used in
  Python comments; used by `pylint3`.
* **Makefile**: a list of `make` targets for building and testing the
  Haskell and Python scripts; see **devnotes.md** for a description of some
  of the most important ones.

TODO

MANIFEST.in
phoneng.cabal
pylintrc
README.md
scripts/csvdict
scripts/decompose
scripts/lytspelify
scripts/mergeprons
scripts/PhonEng.pm
setup.cfg
Setup.hs
setup.py
src/dictbuilder.hs
src/PhonEng.hs
src/pronounce.hs
test/expected-output.epub
test/expected-output.html
test/expected-output.txt
test/expected-output.xhtml
test/icon.png
test/input.epub
test/input.html
test/input.txt
test/input.xhtml
test/Makefile
test/style.css

lytspel/conv.py
lytspel/dict.py
lytspel/__init__.py
lytspel/static/bootstrap.min.css
lytspel/templates/web.html
lytspel/test.py
lytspel/util.py
lytspel/web.cfg
lytspel/web.py

TODO The phoneng program suite is written in
[Haskell](https://www.haskell.org/haskellwiki/Haskell). To build it from
source, you need the [Cabal](https://www.haskell.org/cabal/) build system.
If you use a Debian-based system, install the `cabal-install` package to
get it.

Afterwards clone this repository from GitHub and run the following commands
in the main directory:

    cabal configure && cabal build && cabal install

The compiled programs should now be in your path and ready to run.


## Files and File Formats

All files are in UTF-8 format (some of them may use just the ASCII subset).

*Line files* (extension: .txt) have one entry per line; line breaks in entries
are therefore not allowed.

*Key-value files* (extension: .txt) are line files where each line represents
a key/value pair. Keys and values are separated by ':'; trailing comments
introduced by '#' are stripped. No escape syntax is supported, hence keys
cannot contain ':', values cannot contain '#', and neither can contain line
breaks.


### Files in data Directory

TODO

data/cmudict-phonemes.txt
data/compound-blacklist.csv
data/compounds.csv
data/compounds.csv.bak
data/compounds.csv.old
data/compounds-manual.csv
data/.compounds-manual.csv.swp
data/compound-stress.csv
data/compound-whitelist.csv
data/custom.csv
data/custom.csv.bak
data/dont-convert-pre-stressed-e.csv
data/endpron.csv
data/extra-words.txt
data/lytspel-dict.csv
data/lytspel-dict.csv.bak
data/lytspel-dict.csv.old
data/lytspel-phonemes.txt
data/Makefile
data/moby-phonemes.txt
data/phoneme-map.csv
data/phoneng-espeak-dict.csv
data/phoneng-espeak-dict.csv.bak
data/phonetic-dict.csv
data/phonetic-dict.csv.bak
data/phonetic-dict.txt
data/phonetic-dict.txt.bak
data/prefix.csv
data/prefix.list
data/pre-stressed-vowel.csv
data/startpron.csv
data/suffix.csv
data/suffix.list
data/tagify.csv
data/untagify.csv
data/words-not-in-scowl.txt

TODO Update this section.

  * `cmudict-phonemes.txt`: key-value file containing a mapping from the
    phonemes used in cmudict to the corresponding Phonetic English
    phonemes. Used by the `dictbuilder` program.

  * `custom.csv`: CSV file listing those words for which a specific
    pronunciation should be used. The case of the words listed in the
    first field is ignored. The second field may contain the following
    values:

      * B: use British (RP) pronunciation
      * A: use American (GA) pronunciation
      * P: use the PhonEng pronunciation
      * D: don't add the word to the output dictionary (used for rare
        foreign words or names)
      * O: leave the spelling of the word unchanged
      * Alternatively, a custom pronunciation may be given which is then
        used to generate the final spelling

    Manually created file; used by `lytspelify`.

  * `moby-phonemes.txt`: key-value file containing a mapping from the phonemes
    used in Moby to the corresponding Phonetic English phonemes. Used by the
    `dictbuilder` program.

  * `words-not-in-scowl.txt`: Line file containing words that aren't listed in
    SCOWL but should become part of the pronunciation dictionary. Used by the
    `dictbuilder` program.

  * `phonetic-dict.txt`: Line file containing a mapping from words to their
    pronunciations. If there is just a single pronunciations, the entry is
    written as `word: pron`. If the pronunciation of a word depends on which
    POS (part-of-speed) it is, it is written as `word/n: pron1; v: pron2`
    (where "n", "v" etc. are POS tags). Redirects are written as `word:>
    target`, e.g. `colour:> color`. Generated by the `dictbuilder` program.


## History: Steps used to Generate the Phonetic Dictionary

Some of the following steps require manual intervention. They are described
here to document the history of phoneng.

Downloaded and installed knowledge sources:

  * Downloaded SCOWL and VarCon from [SCOWL And
    Friends](http://wordlist.aspell.net/) -- version 2014.08.11 was used to
    create the distributed dictionary. Unzipped both of them within the `data`
    directory and renamed the resulting subdirectories to `scowl` and `varcon`.
  * Downloaded the [CMU Pronouncing
    Dictionary](http://www.speech.cs.cmu.edu/cgi-bin/cmudict) -- version 0.7a
    was used to create the distributed dictionary. It's enough to download the
    file `cmudict.0.7a` and store it in a new `data` subdirectory named
    `cmudict`.
  * Downloaded the [Moby Pronunciation List by Grady
    Ward](http://www.gutenberg.org/ebooks/3205). Created a `data` subdirectory
    named `moby` and unzipped it there.

Then run `make` from within the `data` directory, that should handle the
rest. TODO Document PATH requirements.

TODO Or execute the following commands manually (the rest of this section
is outdated and should be deleted or possibly cleaned up):

Invoked the `dictbuilder` program within the `data` directory. This writes
a file called `phonetic-dict.txt`.

Invoked the `csvdict` script. This writes a file called
`phoneng-espeak-dict.csv`.

Invoked the `mergeprons` script. This writes a file called `phonetic-dict.csv`.

Invoked the `lytspelify` script. This writes a file called `lytspel-dict.csv`.