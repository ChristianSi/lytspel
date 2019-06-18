# Installation and Usage

## Installation

To install the Lytspel converter locally, two steps are needed:

1. Make sure you have `pip3` (sometimes just called `pip`), the Python
   package installer for Python 3, on your system.
2. Use it to install Lytspel itself.

If you have Python 3.4 or newer on your system, `pip3` should already be
included with it. Otherwise, please [download and
install](https://www.python.org/downloads/) the latest Python version.

Once you have Python and `pip3` available, simply type

    pip3 install lytspel

to install the Lytspel converter.

In some installations, the command may be named `pip` instead of `pip3`. In
that case you'll have to adjust the invocation accordingly. But please make
sure that the used `pip` supports Python 3 rather than 2 (check the output
of `pip --version`).


## Usage

Once installed, you should have `lytspel` in your path, and `lytspel -h`
should print a fairly informative help text.

The most typical usage will likely be to convert one or several files:

    lytspel FILE [FILE ...]

Supported file formats are plain text, HTML, and epub. In case of text and
HTML files, the converted file will be printed to standard output (stdout)
by default. In case of epub files, a suitable file name will be generated
(if the input file is called "mytext.epub", the output file will be called
"mytext-lytspel.epub").

In all cases, you can specify a different output file name by using the
`-o` (or `--outfile`) option:

    lytspel INFILE -o OUTFILE

This only works for a single file at a time, though.

You can specify "-" instead of a file name to convert text read from
standard input (stdin). However, this only works for plain text, not for
HTML or epub. It allows converting the output of other programs by piping
them to `lytspel`:

    cat FILE | lytspel -

(Not very useful in this case, but might be useful in others.)

If you invoke `lytspel -` without piping anything to stdin, the converter
switches into interactive mode: type a line, then press return to see the
converted output. To leave interactive mode and quit the converter, press
`Ctrl-D` (or the local equivalent on your system) to close stdin.

You can also specify any text you want to convert directly on the command
line by using the `-c` (or `--convert`) option:

    lytspel -c "This is the text that should be converted to Lytspel."

In all cases, if you specify the `-u` (or `--unknown`) option, the
converter will also print a list of any unknown words it encountered during
conversion (words not listed in its dictionary). These are often proper
names or typos, but might also indicate words that should be added to the
dictionary. Repeat the option several times to list only words that occur
at least that often (eliminating typos or very rare names) -- e.g. if you
specify `-uuu`, only unknown words that occurred at least 3 times in the
converted texts will be printed.

Finally, if you just want to see the program's version number, type
`lytspel -v` or `lytspel --version`.
