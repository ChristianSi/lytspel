lytspel-dict.csv: compounds.csv compound-stress.csv custom.csv phonetic-dict.csv phoneme-map.csv dont-convert-pre-stressed-e.csv pre-stressed-vowel.csv prefix.csv suffix.csv startpron.csv endpron.csv
	lytspelify

compounds.csv: phonetic-dict.csv compounds-manual.csv compound-blacklist.csv compound-whitelist.csv prefix.list suffix.list
	decompose

phonetic-dict.csv: phoneng-espeak-dict.csv phoneme-map.csv
	mergeprons

phoneng-espeak-dict.csv: phonetic-dict.txt extra-words.txt tagify.csv untagify.csv
	csvdict

phonetic-dict.txt: cmudict-phonemes.txt moby-phonemes.txt words-not-in-scowl.txt cmudict/cmudict.0.7a moby/mpron.txt varcon/varcon.txt
	dictbuilder
