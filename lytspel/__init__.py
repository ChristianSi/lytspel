"""Convert a file or a text block from tradspell to Lytspel."""

import argparse
import readline
from sys import argv
import warnings

from .conv import Converter
from .util import PACKAGENAME


__version__ = '1.1.0'


def compact_warning(message, category, filename, lineno, line=None):
    """Print warnings without showing the warning message line itself."""
    #pylint: disable=unused-argument
    return '{}:{}: {}: {}\n'.format(filename, lineno, category.__name__, message)

warnings.formatwarning = compact_warning


def main() -> None:
    """Run this script."""
    if argv[0] in ('-c', '') or '/' in argv[0]:
        # Set script name if it's not already set or fix it if it contains the full path
        argv[0] = PACKAGENAME
    scriptname = argv[0]

    parser = argparse.ArgumentParser(
        epilog='Specify "-" instead of a FILE to convert text read from stdin. This only '
               'works for plain text, not for HTML or epub. It also works interactively: '
               'enter a line, then press return to see the converted output; press Ctrl-D '
               '(or the local equivalent on your system) to quit.')
    parser.add_argument('files', metavar='FILE', nargs='*',
                        help='file to convert (plain text, HTML, or epub)')
    parser.add_argument('-c', '--convert', metavar='TEXT',
                        help='convert TEXT that follows (any FILES will be ignored)')
    parser.add_argument('-o', '--outfile', metavar='OUTFILE',
                        help='write output to OUTFILE (exactly one input FILE required)')
    parser.add_argument('-u', '--unknown', action='count', default=0,
                        help='list unknown words (repeat option n times to list only words'
                        ' that occur at least that often)')
    parser.add_argument('-v', '--version', action='version',
                        version='{} {}'.format(scriptname, __version__))

    args = parser.parse_args()

    use_unknown_counter = args.unknown > 0
    conv = Converter(use_unknown_counter)

    if args.convert:
        print(conv.convert_para(args.convert, test_if_foreign=False))
    else:
        if not args.files:
            exit('{}: Specify file(s) to convert or use the -c argument.\n'
                 "Try '{} -h' for more information.".format(scriptname, scriptname))

        if args.outfile:
            if not len(args.files) == 1:
                exit('{}: -o/--outfile argument requires exactly 1 input FILE, not {}.\n'
                     "Try '{} -h' for more information.".format(
                         scriptname, len(args.files), scriptname))

        for file in args.files:
            conv.convert_file(file, args.outfile)

    if use_unknown_counter:
        conv.print_unknown_words(args.unknown)
