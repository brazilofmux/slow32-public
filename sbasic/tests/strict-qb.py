#!/usr/bin/env python3
"""strict-qb.py -- the compilable dialect's checker held to QuickBASIC.

The rule (user, 2026-11-03): the compilable dialect is what QuickBASIC
loads, so `sbasic -check` (src/strict.c) must refuse a program exactly
when QBasic 1.1 refuses to load it -- with one deliberate difference, a
DEF FN used before its definition ("Function not defined" in QB), which
the compiler takes.  For each .bas program in the directories given:

    AGREE        both take it, or both refuse it
    DIFF-SB      only the checker refuses it
    DIFF-QB      only QBasic refuses it (at load)
    HOISTED      QBasic's "Function not defined", taken here on purpose

QBasic runs through nbs.py (dos-monster, its cache), each program with
SYSTEM put first: one that loads ends at once, one that does not shows
QBasic's load-time error.

    strict-qb.py DIR [DIR ...]     the disagreements, then the counts
"""
import os, sys, collections
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import nbs
import importlib.util
spec = importlib.util.spec_from_file_location('survey', os.path.join(HERE, 'strict-survey.py'))
survey = importlib.util.module_from_spec(spec)
spec.loader.exec_module(survey)

def qb_refuses(src, exe):
    """QBasic's load-time error for src, or None when it loads: the program
    run with SYSTEM put first, so a program that loads ends at once, and
    one that does not shows the error dialog.  No keys, no output, and no
    telling a load-time message from the same message at run time."""
    v = nbs.run_qbasic(b'SYSTEM\r\n' + src, b'', exe)
    if isinstance(v, str) and v.startswith('ERROR('):
        return v[6:-1]
    # (a file QBasic will not open at all says so before any program
    # runs: "Tried to load file with duplicate procedure definition")
    if isinstance(v, list) and v and v[0].startswith('Tried to load file'):
        return v[0]
    return None


def main():
    dirs = sys.argv[1:]
    if not dirs:
        print(__doc__); sys.exit(2)
    exe = nbs.qbasic_exe()
    counts = collections.Counter()
    for d in dirs:
        for f in sorted(x for x in os.listdir(d) if x.lower().endswith('.bas')):
            path = os.path.join(d, f)
            n, finds, info, err = survey.check(path)
            if n is None:
                counts['unparsed'] += 1
                continue
            qb = qb_refuses(open(path, 'rb').read(), exe)
            if qb and qb.startswith('Function not defined') and info.get('fn-hoisted'):
                counts['HOISTED'] += 1
                continue
            if bool(n) == bool(qb):
                counts['AGREE'] += 1
                continue
            kind = 'DIFF-SB' if n else 'DIFF-QB'
            counts[kind] += 1
            what = '; '.join('%s line %d' % (c, l) for l, c, _ in finds[:3]) if n else qb
            print('%-8s %s: %s' % (kind, os.path.relpath(path), what))
    print('strict-qb: ' + ', '.join('%s %d' % (k, counts[k]) for k in
                                   ('AGREE', 'HOISTED', 'DIFF-SB', 'DIFF-QB', 'unparsed') if counts[k]))
    sys.exit(1 if counts['DIFF-SB'] or counts['DIFF-QB'] else 0)


if __name__ == '__main__':
    main()
