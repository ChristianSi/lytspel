all: txt html xhtml epub outtxt outhtml stdin convert
	true

epub:
	lytspelconv input.epub
	diff expected-output.epub input-lytspel.epub
	rm input-lytspel.epub

outtxt:
	lytspelconv -o temp-output.txt input.txt
	diff -U1 expected-output.txt temp-output.txt
	rm temp-output.txt

outhtml:
	lytspelconv -o temp-output.html input.html
	diff -U1 expected-output.html temp-output.html
	rm temp-output.html

stdin:
	lytspelconv - < input.txt | diff -U1 expected-output.txt -

convert:
	lytspelconv --convert 'The North Wind and the Sun were disputing which was the stronger' | grep -xq 'Dhe North Wind and dhe Sun wur di’spiuting wich wos dhe strongger'

%:
	lytspelconv input.$* | diff -U1 expected-output.$* -
