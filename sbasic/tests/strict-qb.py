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

QBasic runs through nbs.py (dos-monster, its cache); a program's .in or
.input file is its keys, as there, so earlier surveys' runs are reused.
"Refused at load" is read from the message: QB's load-time ones below.

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

LOAD_ERRORS = (
    'NEXT without FOR', 'FOR without NEXT', 'Label not defined', 'Duplicate label',
    'Duplicate definition', 'Array already dimensioned', 'Function not defined',
    'Block IF without END IF', 'END IF without block IF', 'Syntax error', 'Expected',
    'WEND without WHILE', 'WHILE without WEND', 'LOOP without DO', 'DO without LOOP',
    'SELECT without END SELECT', 'CASE without SELECT', 'Subprogram not defined',
)


def qb_refuses(verdict):
    if not isinstance(verdict, str) or not verdict.startswith('ERROR('):
        return None
    msg = verdict[6:-1]
    for m in LOAD_ERRORS:
        if msg.startswith(m):
            return msg
    return None


def keys_for(path):
    base = os.path.splitext(path)[0]
    for ext in ('.in', '.input', '.IN', '.INPUT'):
        if os.path.exists(base + ext):
            return open(base + ext, 'rb').read()
    return b''


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
            src = open(path, 'rb').read()
            v = nbs.run_qbasic(src, keys_for(path), exe)
            if v == 'TIMEOUT':
                # the keys can close a load-time error's dialog (its OK) and
                # leave QBasic's editor; a load error comes before any input
                v = nbs.run_qbasic(src, b'', exe)
            qb = qb_refuses(v)
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
