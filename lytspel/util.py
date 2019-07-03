"""Utility functions for Lytspel."""

from sys import stderr
from typing import Sequence, TypeVar


PACKAGENAME = 'lytspel'

T = TypeVar('T')


def get_elem(seq: Sequence[T], idx: int) -> T:
    """Safely retried an element from a sequence.

    None is returned if 'seq' ends before the requested 'idx' position.
    """
    if len(seq) > idx:
        return seq[idx]
    else:
        return None


def printmsg(msg: str) -> None:
    """Print a message to stderr, prefixed by the name of the script."""
    print('{}: {}'.format(PACKAGENAME, msg), file=stderr)


def readfile(filename: str) -> str:
    """Read a whole file into a string and return it."""
    with open(filename) as file:
        return file.read()
