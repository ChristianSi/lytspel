python = python3  # Or set to python3.5 to test with older version
lytspel = PYTHONPATH=.. $(python) -c 'from lytspel import main; main()'

all: most epub
	@echo Success

most: txt html xhtml outtxt outhtml stdin convert version
	@echo Success

epub:
	$(lytspel) input.epub
	diff expected-output.epub input-lytspel.epub
	rm input-lytspel.epub

outtxt:
	$(lytspel) -o temp-output.txt input.txt
	diff -U1 expected-output.txt temp-output.txt
	rm temp-output.txt

outhtml:
	$(lytspel) -o temp-output.html input.html
	diff -U1 expected-output.html temp-output.html
	rm temp-output.html

stdin:
	$(lytspel) - < input.txt | diff -U1 expected-output.txt -

convert:
	$(lytspel) --convert 'The North Wind and the Sun were disputing which was the stronger' | grep -xq 'The North Wind and the Sun wur dispueting which wos the strongger'

version:
	echo "Checking that script and setup.py show the same version number"
	$(lytspel) --version | cut -f2 -d' ' | grep -f - ../setup.py | grep -q version

%:
	$(lytspel) input.$* | diff -U1 expected-output.$* -
