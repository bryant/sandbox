from functools import wraps
from urllib import urlopen
import re

FIRST_ISSUE = "http://www.newyorker.com/magazine/2007/01/08"

def date_of(s):
    m = re.search(r'\d{4}/\d{2}/\d{2}', s)
    return '' if m is None else m.group(0)

class file_cache(object):
    def __init__(self, args2filename, basedir='.'):
        self.args2filename = args2filename
        self.basedir = basedir

    def __call__(self, func):
        @wraps(func)
        def decorated(*args, **kwargs):
            filename = self.basedir + '/' + self.args2filename(*args, **kwargs)
            try:
                return open(filename).read()
            except IOError:
                rv = func(*args, **kwargs)
                with open(filename, 'w') as f:
                    f.write(rv)
                return rv
        return decorated

@file_cache(lambda iss: date_of(iss).replace('/', '-'), basedir='/tmp')
def issue_contents(iss):
    src = urlopen(iss)
    return '' if src.code != 200 else src.read()

def fetch_issues_since(start, outdir='.'):
    def get_next(iss):
        next = re.search(r'(http://[^"]+?)">Next issue', issue_contents(iss))
        return None if next is None else next.group(1)

    if issue_contents(start) == "":
        return
    print date_of(start)
    issue = start
    while get_next(issue) is not None:
        issue = get_next(issue)
        print date_of(issue)

#@
#def articles_in(issue):
#    l
