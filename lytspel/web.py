"""A web front end for the Lytspel converter."""

from collections import OrderedDict
import os
from os import path
from typing import NamedTuple, Tuple
from uuid import uuid4

from flask import (Flask, flash, Markup, render_template, request, redirect, send_file,
                   send_from_directory)
import misaka
from werkzeug.utils import secure_filename

from .conv import Converter

##### Types #####

PageData = NamedTuple('PageData', [('title', str), ('content', Markup)])


##### Helpers for building immutable values #####

def replace_first_line(text: str, first_line: str) -> str:
    """Replaced the first line in 'text', setting it to 'first_line' instead.

    A modified copy of the text is returned.
    """
    lines = text.splitlines()
    lines[0] = first_line
    return '\n'.join(lines)


def split_at(text: str, sep: str) -> Tuple[str, str]:
    """Split 'text' at the specified separator.

    Returns a 2 tuple: the part before the separator and the rest of the string (starting
    with the separator).

    Raises a ValueError if 'text' does not contain 'sep'.
    """
    parts = text.partition(sep)
    if not parts[1]:
        raise ValueError('"{}" not found in string'.format(sep))
    return parts[0], parts[1] + parts[2]


def markdown_markup(text: str, move_headers_up: bool = False) -> Markup:
    """Parse a text as Markdown and style it for Bootstrap.

    If the second argument is True, all headers are moved one level up: H2 becomes H1,
    H3 becomes H2 etc.
    """
    if move_headers_up:
        text = text.replace('## ', '# ')
    return Markup(misaka.smartypants(misaka.html(text))
                  .replace('&#39;', '&rsquo;')  # smartypants sometimes gets this wrong
                  .replace('<blockquote>', '<blockquote class="alert alert-secondary">')
                  .replace('<pre>', '<pre class="alert alert-primary">'))


def build_page_dict() -> 'OrderedDict[str, PageData]':
    """Build a dictionary from URLs to pre-rendered PageData."""
    result = OrderedDict()  # type: OrderedDict[str, PageData]
    with open('README.md') as file:
        readme = file.read()
    idea, rest = split_at(readme, '## Conventions')
    # Replace title
    idea = replace_first_line(idea, '# Idea and Motivation')
    result['idea'] = PageData('Idea and Motivation', markdown_markup(idea))

    # We render this file now so it will be inserted into the navbar in the desired place
    with open('docs/lytspel-on-one-page.md') as file:
        overview = file.read()
    overview = replace_first_line(overview, '# Lytspel in Brief')
    result['overview'] = PageData('Brief Overview', markdown_markup(overview))

    # Returning to the README
    rules, rest = split_at(rest, '## International')
    # Move the second section header in the Rules fragment up, replacing the first one
    rulelines = rules.splitlines()
    idx = 0
    for idx, line in enumerate(rulelines):
        if idx and line.startswith('## '):
            break
    if idx:
        header_line = rulelines.pop(idx)
        rulelines[0] = header_line
    rules = '\n'.join(rulelines)
    # Boldface first "stress"
    rules = rules.replace('stress', '**stress**', 1)
    result['rules'] = PageData('Complete Ruleset', markdown_markup(rules, True))

    intl, rest = split_at(rest, '## Limitations')
    result['intl'] = PageData('International Pronunciation Differences',
                              markdown_markup(intl, True))

    limitations = split_at(rest, '## Other')[0]
    result['limitations'] = PageData('Dictionary Limitations', markdown_markup(limitations, True))

    with open('INSTALL-USE.md') as file:
        cli = file.read()
    cli = replace_first_line(cli, '# Command-Line Program')
    result['cli'] = PageData('Command-Line Version', markdown_markup(cli))

    return result


def format_nav_item(url: str, title: str) -> str:
    """Format a single entry for the navigation bar (navbar)."""
    return '<li class="nav-item"><a class="nav-link" href="{}">{}</a></li>'.format(url, title)


def render_nav_items(page_dict: 'OrderedDict[str, PageData]') -> Markup:
    """Pre-render navbar items for quick insertion into all pages."""
    itemlist = [format_nav_item('/', 'Converter')]
    for url, page_data in page_dict.items():
        itemlist.append(format_nav_item('/' + url, page_data.title))
    return Markup('\n'.join(itemlist))



##### Constants and immutable values #####

# Note: keep this in sync with the "accept" list in templates/startpage.html
ALLOWED_EXTENSIONS = set([
    'epub', 'htm', 'html', 'markdown', 'md', 'rst', 'txt', 'xht', 'xhtml', 'xml'])

HOME = path.expanduser('~')

MAX_FILE_SIZE_IN_MB = 10
MAX_FILE_SIZE_IN_B = MAX_FILE_SIZE_IN_MB * 1024 * 1024

PAGE_DICT = build_page_dict()

NAV_ITEMS = render_nav_items(PAGE_DICT)


##### App config #####

app = Flask(__name__)  # pylint: disable=invalid-name
app.config.from_pyfile('web.cfg')

# Set suitable default values
app.config.setdefault('UPLOAD_FOLDER', HOME + '/webdata/uploads')


@app.before_request
def clear_trailing():
    """Strip spurious slashes from all requests."""
    first_char, rest = request.path[:1], request.path[1:]
    if first_char == '/' and (rest.endswith('/') or rest.startswith('/')):
        ## Strip any slashes at the end and any (except the very first) at the start
        rest = rest.strip('/')
        return redirect(first_char + rest)
    else:
        return None  # let request pass as is


##### App endpoints #####

@app.route("/", methods=['GET', 'POST'])
def web() -> str:
    """Main entry point."""
    tradspell = ''
    lytspel = ''

    if request.method == 'POST':
        tradspell = request.form['tradspell']

        # Enforce maximum length to prevent DOS attacks
        if len(tradspell) > 21000:
            tradspell = tradspell[:21000]

    if tradspell:
        conv = Converter()
        lytspel = conv.convert_para(tradspell, False)

    return render_template('startpage.html',
                           nav_items=NAV_ITEMS, form=request.form,
                           tradspell=tradspell, lytspel=lytspel)


@app.route("/file", methods=['GET', 'POST'])
def convert_file() -> str:
    """Convert a file."""
    # pylint: disable=too-many-return-statements
    if request.method == 'POST':

        # Check if the post request has the file part
        if 'file' not in request.files:
            return redirect_with_error('No selected file', request.url)
        file = request.files['file']

        # If user does not select a file, browser submits an empty part without filename
        if file.filename == '':
            return redirect_with_error('No selected file', request.url)

        if not allowed_file(file.filename):
            return redirect_with_error(
                'Unsupported file type (please select a text, HTML, or epub document)',
                request.url)

        # Determine file extension and name for output file
        source_name = secure_filename(file.filename)
        root, ext = path.splitext(source_name)
        target_name = '{}-lytspel{}'.format(root, ext)

        # Save file locally using a random name (otherwise there might be collisions)
        uid = str(uuid4())
        upload_folder = app.config['UPLOAD_FOLDER']
        in_file_name = '{}-in{}'.format(uid, ext)
        in_file_path = path.join(upload_folder, in_file_name)
        out_file_name = '{}-out{}'.format(uid, ext)
        out_file_path = path.join(upload_folder, out_file_name)
        file.save(in_file_path)

        # Delete rather than converting files that are too large and return an error message
        # (we don't use Flask's MAX_CONTENT_LENGTH setting since that aborts connections
        # in a user-unfriendly way)
        if os.stat(in_file_path).st_size > MAX_FILE_SIZE_IN_B:
            os.remove(in_file_path)
            return redirect_with_error(
                'File too large (at most {} MB are allowed)'.format(MAX_FILE_SIZE_IN_MB),
                request.url)

        # Convert file and offer it for download
        conv = Converter()
        try:
            conv.convert_file(in_file_path, out_file_path)
        except Exception as err:   # pylint: disable=broad-except
            return redirect_with_error('Could not convert file: {}'.format(err), request.url)

        return send_from_directory(
            upload_folder, out_file_name, as_attachment=True, attachment_filename=target_name,
            cache_timeout=0, add_etags=False)

    # GET: redirect to start view
    return redirect('/')


@app.route("/lytspel-on-one-page.pdf", methods=['GET'])
def one_page_pdf() -> str:
    """Serve the requested PDF document."""
    return send_file('../docs/lytspel-on-one-page.pdf', mimetype='application/pdf')


@app.route("/<url>", methods=['GET'])
def doc_page(url: str) -> str:
    """Show a page from the documentation."""
    page_data = PAGE_DICT.get(url)
    if page_data:
        return render_template(
            'base.html', nav_items=NAV_ITEMS, content=page_data.content, title=page_data.title)
    else:
        # Redirect to intended page if letter case is the problem, otherwise to start page
        target_url = '/'
        if not url.islower():
            lower_cased = url.lower()
            if lower_cased in PAGE_DICT:
                target_url += lower_cased
        return redirect(target_url)


##### Helper functions #####

def redirect_with_error(msg: str, url: str) -> str:
    """"Redirect to specific URL, flashing an error message."""
    flash(msg)
    return redirect(url)


def allowed_file(filename: str) -> bool:
    """Check whether an uploaded file has one of the supported extensions."""
    return '.' in filename and filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS


if __name__ == "__main__":
    app.run()
