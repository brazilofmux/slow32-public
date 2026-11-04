#!/usr/bin/env python3
"""qb-corpus.py -- s32-bc over real QuickBASIC programs, by what stops it.

For each .bas program under the directories given (default: QB64-PE's
QBasic test cases, ~/refs/basic/qb64pe/tests/qbasic_testcases -- not in
this tree), QBasic 1.1 says whether it loads (nbs.py, the program run
with SYSTEM its first statement, nbs.load_probe), and s32-bc says what it makes of it:

    COMPILES        s32-bc compiles it
    PARSE           the parser (sbasic's) refuses it: syntax QuickBASIC has
    CHECK           -check refuses it (a QBasic program: the checker is wrong)
    NOT-YET         s32-bc does not do something in it yet
    OTHER           anything else (a limit, a crash)

Programs QBasic will not load (QB64's own extensions, and the like) are
counted apart and not listed.  The reasons are then counted, the work
list in the order it pays.

    qb-corpus.py [-v] [DIR ...]
"""
import collections
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
SBC = os.path.dirname(HERE)
ROOT = os.path.dirname(SBC)
sys.path.insert(0, os.path.join(ROOT, 'sbasic/tests'))
import nbs  # noqa: E402

DEFAULT = os.path.expanduser('~/refs/basic/qb64pe/tests/qbasic_testcases')
S32BC = os.path.join(SBC, 's32-bc')


def qb_loads(src, exe):
    """None if QBasic loads it, else its complaint"""
    v = nbs.run_qbasic(nbs.load_probe(src), b'', exe)
    if isinstance(v, str) and v.startswith('ERROR('):
        return v[6:-1]
    if isinstance(v, list) and v and v[0].startswith('Tried to load file'):
        return v[0]
    return None


def bc_verdict(path):
    try:
        r = subprocess.run([S32BC, path, os.devnull], capture_output=True, timeout=60)
    except subprocess.TimeoutExpired:
        return 'OTHER', 'timeout'
    msg = r.stderr.decode('latin-1').strip().split('\n')[0] if r.stderr else ''
    if r.returncode == 0 and not msg:
        return 'COMPILES', ''
    m = re.match(r's32-bc: line \d+: (.*)', msg)
    why = m.group(1) if m else msg
    if why.startswith('not yet'):
        return 'NOT-YET', why
    if re.match(r'[a-z]+(-[a-z]+)+:', why) or re.match(r'[a-z]+-[a-z]+:', why):
        return 'CHECK', why.split(':')[0]
    if 'Syntax error' in why or 'Unexpected' in why or 'Expected' in why or 'not supported' in why:
        return 'PARSE', why
    return 'OTHER', why


def main():
    args = sys.argv[1:]
    verbose = '-v' in args
    dirs = [a for a in args if a != '-v'] or [DEFAULT]
    exe = nbs.qbasic_exe()
    progs = []
    for d in dirs:
        for root, _, files in os.walk(d):
            progs += [os.path.join(root, f) for f in files if f.lower().endswith('.bas')]
    counts, reasons, notqb = collections.Counter(), collections.Counter(), 0
    for p in sorted(progs):
        src = open(p, 'rb').read()
        if qb_loads(src, exe):
            notqb += 1
            continue
        verdict, why = bc_verdict(p)
        counts[verdict] += 1
        if verdict != 'COMPILES':
            reasons['%-9s %s' % (verdict, why)] += 1
        if verbose or verdict not in ('COMPILES',):
            print('%-9s %s  %s' % (verdict, os.path.relpath(p, os.path.commonpath(dirs)), why))
    print('QBasic loads %d of %d: %s' % (sum(counts.values()), len(progs),
          ', '.join('%s %d' % (k, counts[k]) for k in ('COMPILES', 'PARSE', 'CHECK', 'NOT-YET', 'OTHER')
                    if counts[k])))
    for k, n in reasons.most_common():
        print('    %4d  %s' % (n, k))


if __name__ == '__main__':
    main()
