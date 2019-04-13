"""A web front end for the Lytspel converter."""

from flask import Flask, render_template, flash, request

from .conv import Converter


# App config
app = Flask(__name__)  # pylint: disable=invalid-name
app.config.from_pyfile('web.cfg')
app.config['SECRET_KEY'] = 'vi_kajBer2racag`6Okphub'

@app.route("/", methods=['GET', 'POST'])
def web() -> str:
    """Main entry point."""
    tradspell = ''
    if request.method == 'POST':
        tradspell = request.form['tradspell']

    if tradspell:
        conv = Converter()
        flash(conv.convert_para(tradspell, False))

    return render_template('web.html', form=request.form)


if __name__ == "__main__":
    app.run()
