from argparse import ArgumentParser
from os import path
import re

args = ArgumentParser(description="Decode git objects")
args.add_argument("-d", required=False, default=None)
args.add_argument("hashid")

def parse_obj(raw):
    pass

def parse_header(raw):
    objty, length, rest = re.match(r"^(\S+) (\d+)\x00(.*)", raw, re.DOTALL)\
                            .groups()
    length = int(length)
    assert(length == len(rest))
    return objty, length, rest

def parse_entry(raw):
    s = re.match(r"^(\d+) ([^\x00]+)\x00(.{20})(.*)", raw, re.DOTALL).groups()
    perm, name, sha1, rest = s
    return ((perm, name, sha1.encode("hex")), rest)

def parse_blob(raw):
    return raw

def parse_tree(rest):
    entries = []
    while rest != "":
        entry, rest = parse_entry(rest)
        entries.append(entry)
    return entries

def parse_commit(self, raw):
    return raw

class UnknownObjectType(Exception): pass

class GitDir(object):
    def __init__(self, gitdir="./.git"):
        self.gitdir = gitdir

    def get_obj(self, sha1):
        raw = self._find_obj(sha1)
        objty, length, rest = parse_header(raw)
        if objty == "tree":
            stuff = parse_tree(rest)
        elif objty == "commit":
            stuff = parse_commit(rest)
        elif objty == "blob":
            stuff = parse_blob(rest)
        else:
            raise UnknownObjectType(repr(raw))
        return (objty, stuff, length)

    def _find_obj(self, sha1):
        assert(len(sha1) == 40)
        dir_, file_ = sha1[:2], sha1[2:]
        p = path.join(self.gitdir, "objects", dir_, file_)
        return open(p).read().decode("zlib")

def decode_file(raw):
    pass

if __name__ == "__main__":
    from pprint import pprint

    ns = args.parse_args()
    g = GitDir(ns.d) if ns.d else GitDir()
    pprint(g.get_obj(ns.hashid))
