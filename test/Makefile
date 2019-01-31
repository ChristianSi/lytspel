all: test

test:
	lytspelconv input.txt | diff -U1 expected-output.txt -
	lytspelconv input.html | diff -U1 expected-output.html -
