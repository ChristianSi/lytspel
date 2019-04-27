# Development Notes

## Run lytspel directly from source

The normal way to install lytspel is to run

    pip3 install lytspel

or

    pip install lytspel

(depending on how the pip command for Python 3 is called on your system or
in your virtual environment) and then invoke the `lytspel` command created
during installation.

But if you want to run it directly from the source repository (say for
testing purposes), you can define a `lytspel` alias in your ~/.bashrc (or
suitably adapted for which ever shell you are using):

    alias lc="PYTHONPATH=SOURCEDIR python3 -c 'from lytspel import main; main()'"

Replace SOURCEDIR with the root of your Lytspel repository (i.e., the
directory containing this file).


## Run all checks and tests defined for Python

Change to the root of your Lytspel repository and call

    make


## Publish a new release on PyPI

Note: Only the maintainer should do this.

Steps:

* Bump the version number in `setup.py` and `lytspel/__init__.py`. Versions
  have the form x.y.z.

      * Bump z if the dictionary has changed, bugs have been fixed, or
        other small changes that don't add new functionality have been
        made.
      * Bumpy y (and set z to 0) if new non-trivial functionality has been
        added in a backwards-compatible manner.
      * Bump x (and set y and z to 0) if functionality has added in a way
        that breaks compatibility. NOTE: Whenever possible, this should be
        avoided!

* Update the CHANGELOG.

* Commit all changes and push them to GitHub.

* Delete the old binary and source packages:

        rm -f dist/lytspel-*.*

* Build the new packages:

        make wheel

    Note: this also makes sure that all checks and tests pass. If the
    command aborts with an error, the problem must first be addressed.

* If there is any doubt whether the packages work as they should, install
  them locally using a venv (virtual environment) and try them out.

* Upload them to PyPI:

        twine upload dist/lytspel-*.*
