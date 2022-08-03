"""Utility functions for Lytspel."""

from sys import stderr
from typing import Optional, Sequence, TypeVar


PACKAGENAME = 'lytspel'

T = TypeVar('T')  # pylint: disable=invalid-name


def get_elem(seq: Sequence[T], idx: int) -> Optional[T]:
    """Safely retried an element from a sequence.

    None is returned if 'seq' ends before the requested 'idx' position.
    """
    # pylint: disable=no-else-return
    if len(seq) > idx:
        return seq[idx]
    else:
        return None


def printmsg(msg: str) -> None:
    """Print a message to stderr, prefixed by the name of the script."""
    print(f'{PACKAGENAME}: {msg}', file=stderr)


def readfile(filename: str) -> str:
    """Read a whole file into a string and return it."""
    with open(filename, encoding='utf8') as file:
        return file.read()
