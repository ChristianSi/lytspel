all: txt html xhtml
	true

%:
	lytspelconv input.$* | diff -U1 expected-output.$* -
