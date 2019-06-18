# Development Notes

## Run lytspel directly from source

The normal way to install `lytspel` is to run

    pip3 install lytspel

or

    pip install lytspel

(depending on how the pip command for Python 3 is called on your system or
in your virtual environment) and then invoke the `lytspel` command created
during installation.

But if you want to run it directly from the source repository (say for
testing purposes), you can define a `lytspel` alias in your `~/.bashrc` (or
suitably adapted for whichever shell you are using):

    alias lytspel="PYTHONPATH=SOURCEDIR python3 -c 'from lytspel import main; main()'"

Replace SOURCEDIR with the root of your Lytspel repository (i.e., the
directory containing this file).


## Run all checks and tests defined for Python

Change to the root of your Lytspel repository and call

    make


## Starting the web app

In order to run the web app locally, you first need to create a file called
"web.cfg" in the "lytspel" directory which contains the line:

    SECRET_KEY = '...'

Replace `...` by a dozen or more random characters. That file has to exist
but isn't tracked by git since otherwise the SECRET_KEY wouldn't be secret!

Ensure that the additional dependencies needed for the web app are installed:

    pip3 install flask misaka

If you run the app from source rather than from a pip-installed package,
you also need to ensure that all the dependencies listed in the
"install_requires" section of the "setup.py" file are installed.

You also need to create a directory where the app can temporarily store
uploaded files if it doesn't exist already. By default,
"$HOME/webdata/uploads" is used for this purpose (where $HOME is your
home directory). If you want to use another directory, you can add an entry
`UPLOAD_FOLDER = '...'` to your "web.cfg".

Note that the temporary files created in this directory are **not** deleted
by the web app. You might delete them manually after stopping the app, or
preferably add an entry such as

    */15  *  *  *  *  find ~/webdata/uploads/ -type f -mmin +45 -delete

to your `crontab`. This will ensure that files within the specified
directory older than 45 minutes will be deleted every 15 minutes (so no
file will survive for more than one hour).

Afterwards, change to the root of your Lytspel repository and call

    make flask

to start the web app.

To stop it, press `Ctrl+C`.


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
      * Bump x (and set y and z to 0) if functionality has been added in a
        way that breaks compatibility. NOTE: Whenever possible, this should
        be avoided!

* Update the CHANGELOG.

* Commit all changes and push them to GitHub.

* Delete the old binary and source packages:

        rm -f dist/lytspel-*.*

* Build the new packages:

        make wheel

    Note: this also makes sure that all checks and tests pass. If the
    command aborts with an error, the problem must first be addressed.

* If there is any doubt whether the packages work as they should, install
  them locally using a [venv](https://docs.python.org/3/tutorial/venv.html)
  (virtual environment) and try them out.

* Upload them to PyPI:

        twine upload dist/lytspel-*.*
