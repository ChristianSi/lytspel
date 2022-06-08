"""A web front end for the Lytspel converter."""
# pylint: disable=consider-using-f-string

from collections import OrderedDict
from glob import glob
import os
from os import path
import logging
import re
import string
from time import time

from typing import Match, NamedTuple, Optional, Sequence, Tuple
# The following import is for mypy only
from typing import Dict  #pylint: disable=unused-import
from uuid import uuid4

from flask import (Flask, flash, make_response, Markup, render_template, request, redirect,
                   send_file, send_from_directory)
from flask.logging import create_logger
import misaka
from werkzeug.utils import secure_filename
from werkzeug.wrappers import Response

from .conv import Converter
from .util import readfile

##### Types #####

PageData = NamedTuple('PageData', [('title', str), ('content', Markup)])

SamplePara = NamedTuple('SamplePara', [('tradspell', Markup), ('lytspel', Markup)])

SamplePageData = NamedTuple('SamplePageData', [('title', str), ('paras', Sequence[SamplePara])])


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


def hexify_string(text: str) -> str:
    """Replace each character in a string by the corresponding HTML character entity."""
    return ''.join('&#x{:x};'.format(ord(c)) for c in text)

def text_to_id(text: str) -> str:
    """Convert a text (section title or similar) into an ID.

    * Punctuation and outer whitespace is stripped.
    * Each inner whitespace sequence is converted to a hyphen.
    * Letters are converted to lowercase.
    """
    text = text.strip().lower()
    text = text.translate(str.maketrans('', '', string.punctuation))
    return '-'.join(text.split())

def add_section_anchor(matchobj: Match) -> str:
    """Add anchors (IDs) to section headers.

    Note: This is quite simple and will NOT handle repeated sections with the same title
    correctly.
    """
    idtext = text_to_id(matchobj.group(1))
    return matchobj.group(0).replace('>', f' id="{idtext}">', 1)

def protect_mailto(matchobj: Match) -> str:
    """Helper function that spam-protects mailto links."""
    target = hexify_string(matchobj.group(1))
    linktext = matchobj.group(2)

    if '@' in linktext:
        linktext = hexify_string(linktext)
    return '<a href="mailto:{}">{}</a>'.format(target, linktext)


def markdown_markup(text: str,
                    move_headers_up: bool = False,
                    protect_email_addresses: bool = False) -> Markup:
    """Parse a text as Markdown and style it for Bootstrap.

    If 'move_headers_up' is True, all headers are moved one level up: H2 becomes H1,
    H3 becomes H2 etc.

    If 'protect_email_addresses' is True, mailto links in the generated HTML are obfuscated
    to make it a bit harder for spammers to harvest email addresses.
    """
    if move_headers_up:
        text = text.replace('## ', '# ')

    html = (misaka.smartypants(misaka.html(text))
            .replace('&#39;', '&rsquo;')  # smartypants sometimes gets this wrong
            .replace('&reg;', '(r)')      # revert unwanted replacement
            .replace('<blockquote>', '<blockquote class="alert alert-secondary">')
            .replace('<pre>', '<pre class="alert alert-primary">'))

    # Add anchors to section headers
    html = re.sub('<h2>(.*?)</h2>', add_section_anchor, html)
    if protect_email_addresses:
        html = re.sub('<a href="mailto:([^"]+)">([^<]+)</a>', protect_mailto, html)
    return Markup(html)


def extract_title_from_text(text: str) -> str:
    """Extract and return the title line from a text written in Markdown.

    Returns the first line of the original text, minus any header markup ('#') at the start
    of the line.
    """
    firstline = text.split('\n', 1)[0]
    return firstline.lstrip('# ')


def build_page_dict() -> 'OrderedDict[str, PageData]':
    """Build a dictionary from URLs to pre-rendered PageData."""
    # pylint: disable=too-many-locals
    result = OrderedDict()  # type: OrderedDict[str, PageData]
    readme = readfile('README.md')
    idea, rest = split_at(readme, '## The Rules of Lytspel')
    # Replace title
    title = 'Idea and Motivation'
    idea = replace_first_line(idea, '# ' + title)
    result['idea'] = PageData(title, markdown_markup(idea))

    # We render this file now so it will be inserted into the navbar in the desired place
    overview = readfile('docs/lytspel-on-two-pages.md')
    overview = replace_first_line(overview, '# Lytspel in Brief')
    result['overview'] = PageData('Brief Overview', markdown_markup(overview))

    # Returning to the README
    rules, rest = split_at(rest, '## International')
    result['rules'] = PageData('Complete Ruleset', markdown_markup(rules, move_headers_up=True))

    intl, rest = split_at(rest, '## Limitations')
    title = extract_title_from_text(intl)
    # Add link to earlier page
    intl = intl.replace('have already been motivated', '[have already been motivated](/rules)', 1)
    result['intl'] = PageData(title, markdown_markup(intl, move_headers_up=True))

    limitations = split_at(rest, '## Other')[0]
    result['limitations'] = PageData('Dictionary Limitations',
                                     markdown_markup(limitations, move_headers_up=True))

    cli = readfile('INSTALL-USE.md')
    cli = replace_first_line(cli, '# Command-Line Program')
    result['cli'] = PageData('Command-Line Version', markdown_markup(cli))

    # Add files from webfiles directory in desired order
    for basename in ('privacy', 'contact'):
        text = readfile('webfiles/{}.md'.format(basename))
        title = extract_title_from_text(text)
        result[basename] = PageData(title, markdown_markup(text, protect_email_addresses=True))

    return result


def build_sample_dict() -> 'Dict[str, SamplePageData]':
    """Build a dictionary from URLs to the contents of sample pages."""
    result = OrderedDict()  # type: Dict[str, SamplePageData]
    conv = Converter()

    for filename in glob('samples/*.md'):
        basename = path.splitext(path.basename(filename))[0]
        paras = []
        title = None

        if basename == 'NOTE':
            continue  # Skip (not a sample)

        orig_text = readfile(filename)
        orig_paras = re.split('\n\n', orig_text)  # Split text into paragraphs

        for orig_para in orig_paras:
            conv_para = conv.convert_para(orig_para)
            paras.append(SamplePara(markdown_markup(orig_para), markdown_markup(conv_para)))

            if not title:
                # Extract page title from the converted text of the first paragraph
                title = extract_title_from_text(conv_para)
        result[basename] = SamplePageData(title, paras)
    return result


def format_nav_item(url: str, title: str) -> str:
    """Format a single entry for the navigation bar (navbar)."""
    return '<li class="nav-item"><a class="nav-link" href="{}">{}</a></li>'.format(url, title)


def render_nav_items(page_dict: 'OrderedDict[str, PageData]',
                     sample_dict: 'Dict[str, SamplePageData]') -> Markup:
    """Pre-render navbar items for quick insertion into all pages."""
    itemlist = [format_nav_item('/', 'Converter')]
    for url, page_data in page_dict.items():
        itemlist.append(format_nav_item('/' + url, page_data.title))

    itemlist.append('<li class="nav-item">'
                    '<span class="navbar-text navbar-sep border-top border-2 border-info">'
                    'Samples:</span></li>')
    for local_url in sorted(sample_dict.keys()):
        sample_page_data = sample_dict[local_url]
        itemlist.append(format_nav_item('/sample/' + local_url, sample_page_data.title))

    return Markup('\n'.join(itemlist))



##### Constants and immutable values #####

# A mapping from allowed file extensions to their MIME types.
# Note: keep this in sync with the "accept" list in templates/startpage.html.
ALLOWED_EXTENSIONS = {
    'epub': 'application/epub+zip',
    'htm': 'text/html',
    'html': 'text/html',
    'markdown': 'text/markdown',
    'md': 'text/markdown',
    'rst': 'text/x-rst',
    'txt': 'text/plain',
    'xht': 'application/xhtml+xml',
    'xhtml': 'application/xhtml+xml',
    'xml': 'application/xml'
    }

HOME = path.expanduser('~')

MAX_FILE_SIZE_IN_MB = 10
MAX_FILE_SIZE_IN_B = MAX_FILE_SIZE_IN_MB * 1024 * 1024

# How long (in seconds) HTML pages (except the dynamic start page) should be cached)
HTML_MAX_AGE = 3*60*60  # 3 hours

MULTIPLE_SLASHES_RE = re.compile('//+')

PAGE_DICT = build_page_dict()

SAMPLE_DICT = build_sample_dict()

INTRO_TEXT = markdown_markup(readfile('webfiles/intro.md'))

NAV_ITEMS = render_nav_items(PAGE_DICT, SAMPLE_DICT)

SERVER_SOFTWARE = os.environ.get('SERVER_SOFTWARE', 'flask')


##### App config #####

app = Flask(__name__)  # pylint: disable=invalid-name
app.config.from_pyfile('web.cfg')
LOG = create_logger(app)

# Set suitable default values
app.config.setdefault('UPLOAD_FOLDER', HOME + '/webdata/uploads')

# Configure logging
if 'gunicorn' in SERVER_SOFTWARE:
    gunicorn_logger = logging.getLogger('gunicorn.error')  # pylint: disable=invalid-name
    LOG.handlers = gunicorn_logger.handlers
    LOG.setLevel(gunicorn_logger.level)

LOG.info('App ready to serve under %s', SERVER_SOFTWARE)


@app.before_request
def normalize_url() -> Optional[Response]:
    """Convert all request URLs to lower-case and strip spurious slashes."""
    new_path = orig_path = request.path
    first_char, rest = new_path[:1], new_path[1:]

    if first_char == '/' and (rest.endswith('/') or rest.startswith('/')):
        # Strip any slashes at the end and any (except the very first) at the start
        rest = rest.strip('/')
        new_path = first_char + rest

    if '//' in new_path:
        # Replace repeated slashes by a single one
        new_path = MULTIPLE_SLASHES_RE.sub('/', new_path)

    if not new_path.islower():
        # Convert to lower case (we never use upper-case letters in URLs)
        new_path = new_path.lower()

    if new_path != orig_path:
        log_web_event('URL normalization: Redirecting from %s to %s', orig_path, new_path)
        return redirect(new_path)
    else:
        return None  # let request pass as is


@app.errorhandler(404)
def page_not_found(err):
    """Return requests for non-existing pages to the start page."""
    #pylint: disable=unused-argument
    log_web_event('%s: Redirecting to start page', request.path)
    return redirect('/')


##### App endpoints #####

@app.route("/", methods=['GET', 'POST'])
def startpage() -> str:
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
        log_web_event('/: Converted %d to %d characters', len(tradspell), len(lytspel))
    else:
        log_web_event()

    return render_template('startpage.html',
                           nav_items=NAV_ITEMS, form=request.form, intro=INTRO_TEXT,
                           tradspell=tradspell, lytspel=lytspel)


@app.route("/file", methods=['GET', 'POST'])
def convert_file() -> Response:
    """Convert a file."""
    # pylint: disable=too-many-return-statements
    if request.method == 'POST':
        # Check if the post request has the file part
        if 'file' not in request.files:
            return redirect_with_error('No selected file')
        file = request.files['file']

        # If user does not select a file, browser submits an empty part without filename
        if file.filename == '':
            return redirect_with_error('No selected file')

        if not allowed_file(file.filename):
            return redirect_with_error(
                'Unsupported file type (please select a text, HTML, or epub document)')

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
                'File too large (at most {} MB are allowed)'.format(MAX_FILE_SIZE_IN_MB))

        # Convert file and offer it for download
        conv = Converter()
        try:
            conv.convert_file(in_file_path, out_file_path)
        except Exception as err:   # pylint: disable=broad-except
            return redirect_with_error('Could not convert file: {}'.format(err))

        norm_ext = ext[1:].lower()
        log_web_event('/file: Converted %s file with %d bytes to one with %d bytes',
                      norm_ext, path.getsize(in_file_path), path.getsize(out_file_path))
        return send_from_directory(
            upload_folder, out_file_name, as_attachment=True, attachment_filename=target_name,
            mimetype=ALLOWED_EXTENSIONS.get(norm_ext), cache_timeout=0, add_etags=False)

    # GET: redirect to start view
    log_web_event('/file GET: Redirecting to start page')
    return redirect('/')


@app.route("/favicon.ico", methods=['GET'])
def favicon() -> Response:
    """Redirect old browsers which may expect the favicon in the root."""
    log_web_event('/favicon.ico: Redirecting to /static/favicon.ico')
    return redirect('/static/favicon.ico')


@app.route("/lytspel-on-two-pages.pdf", methods=['GET'])
def two_page_pdf() -> Response:
    """Serve the requested PDF document."""
    log_web_event()
    return send_file('../docs/lytspel-on-two-pages.pdf', mimetype='application/pdf')


@app.route("/robots.txt", methods=['GET'])
def robots_txt() -> Response:
    """Serve the robots.txt file."""
    log_web_event()
    return send_file('../webfiles/robots.txt', mimetype='text/plain')


@app.route("/<localpath>", methods=['GET'])
def doc_page(localpath: str) -> Response:
    """Show a page from the documentation."""
    page_data = PAGE_DICT.get(localpath)
    if page_data:
        log_web_event()
        return cacheable(render_template(
            'base.html', nav_items=NAV_ITEMS, content=page_data.content, title=page_data.title))
    else:
        log_web_event('%s: Redirecting to start page', request.path)
        return redirect('/') # Redirect to start page


@app.route("/sample/<localpath>", methods=['GET'])
def sample_page(localpath: str) -> Response:
    """Show a text sample."""
    page_data = SAMPLE_DICT.get(localpath)
    if page_data:
        log_web_event()
        return cacheable(render_template(
            'sample.html', nav_items=NAV_ITEMS, title=page_data.title, paras=page_data.paras))
    else:
        log_web_event('%s: Redirecting to start page', request.path)
        return redirect('/') # Redirect to start page


##### Helper functions #####

def log_web_event(msg: str = None, *args):
    """Log an info message in the context of a web request.

    Any 'args' are merged into 'msg' using the string formatting operator. Additionally, the
    user agent making the request will be added to the logged message.

    If 'msg' is omitted, a default message noticing that the current request path was fetched
    will be logged.
    """
    # pylint: disable=keyword-arg-before-vararg
    if not msg:
        msg = '%s fetched'
        args = (request.path,)

    msg += ' (user agent: %s)'
    agent = request.user_agent
    agent_string = '{}/{} {}'.format(
        agent.platform or '-', agent.browser or '-', agent.version or '-')  # type: ignore

    if agent_string == '-/- -':
        # Log the raw User-Agent header instead (if sent)
        agent_string = '"{}"'.format(agent.string or '')

    args = (*args, agent_string)
    LOG.info(msg, *args)


def redirect_with_error(msg: str, url: str = '/') -> Response:
    """"Redirect to specific URL, flashing an error message."""
    log_web_event('Error redirect to %s with flash message: %s', url, msg)
    flash(msg)
    return redirect(url)


def allowed_file(filename: str) -> bool:
    """Check whether an uploaded file has one of the supported extensions."""
    return '.' in filename and filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS


def cacheable(resp_body: str, cache_timeout: int = HTML_MAX_AGE) -> Response:
    """Add Cache-Control headers to a response, indicating that it may be cached."""
    resp = make_response(resp_body)
    resp.cache_control.public = True
    resp.cache_control.max_age = cache_timeout
    resp.expires = int(time() + cache_timeout)
    return resp


if __name__ == '__main__':
    app.run()
