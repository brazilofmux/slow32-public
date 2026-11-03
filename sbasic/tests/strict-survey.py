#!/usr/bin/env python3
"""strict-survey.py -- how much of a corpus the compilable dialect takes.

Runs `sbasic -check` (src/strict.c) over every .bas file of each corpus
given and tabulates: programs clean, programs with findings, which codes
reject how many programs, and how many programs use each paid-for
feature.  It is the survey docs/plans/basic-compiler.md asks for before
the dialect's rules are fixed.

    strict-survey.py DIR [DIR ...]          a table per directory and a total
    strict-survey.py -v DIR                 every program's findings too
"""
import os, re, subprocess, sys, collections

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, '..', '..'))
EMU = os.environ.get('EMU', os.path.join(ROOT, 'tools', 'emulator', 'slow32-fast'))
SBASIC = os.path.join(ROOT, 'sbasic', 'sbasic.s32x')

FIND = re.compile(r'^line (\d+): ([a-z-]+): (.*)$')
SUMMARY = re.compile(r'^check: (\d+) findings?;(.*)$')


def check(path):
    r = subprocess.run([EMU, SBASIC, '-check', path], capture_output=True, timeout=120)
    out = r.stdout.decode('latin-1')
    finds, info, summary = [], {}, None
    for line in out.splitlines():
        m = FIND.match(line)
        if m:
            finds.append((int(m.group(1)), m.group(2), m.group(3)))
            continue
        m = SUMMARY.match(line)
        if m:
            summary = int(m.group(1))
            for kv in m.group(2).split():
                k, v = kv.split('=')
                info[k] = int(v)
    if summary is None:          # a parse error: the interpreter refuses it too
        err = [l for l in out.splitlines() if l.startswith('Error')]
        return None, [], {}, err[0] if err else out.strip().splitlines()[-1:]
    return summary, finds, info, None


def survey(d, verbose):
    files = sorted(f for f in os.listdir(d) if f.lower().endswith('.bas'))
    clean = 0
    unparsed = []
    by_code = collections.Counter()      # programs rejected by each code
    hits = collections.Counter()         # findings of each code
    only = collections.Counter()         # programs whose only code it is
    used = collections.Counter()         # programs using each paid-for feature
    for f in files:
        n, finds, info, err = check(os.path.join(d, f))
        if n is None:
            unparsed.append((f, err))
            continue
        codes = {c for _, c, _ in finds}
        for c in codes: by_code[c] += 1
        for _, c, _ in finds: hits[c] += 1
        if len(codes) == 1: only[next(iter(codes))] += 1
        for k in info: used[k] += 1
        if not finds: clean += 1
        elif verbose:
            print('  %s:' % f)
            for line, c, det in finds: print('    line %d: %s: %s' % (line, c, det))
    total = len(files) - len(unparsed)
    print('%s: %d programs, %d clean, %d with findings%s' % (
        d, total, clean, total - clean,
        ', %d do not parse' % len(unparsed) if unparsed else ''))
    for c, k in by_code.most_common():
        print('    %-17s %3d programs (%d alone), %d findings' % (c, k, only[c], hits[c]))
    if used:
        print('    paid for: ' + ', '.join('%s %d' % (k, v) for k, v in used.most_common()))
    return total, clean, by_code, only, used


def main():
    args = sys.argv[1:]
    verbose = False
    if args and args[0] == '-v':
        verbose = True
        args = args[1:]
    if not args:
        print(__doc__); sys.exit(2)
    T = C = 0
    B, O, U = collections.Counter(), collections.Counter(), collections.Counter()
    for d in args:
        t, c, b, o, u = survey(d, verbose)
        T += t; C += c; B += b; O += o; U += u
    if len(args) > 1:
        print('TOTAL: %d programs, %d clean (%.0f%%)' % (T, C, 100.0 * C / max(T, 1)))
        for c, k in B.most_common():
            print('    %-17s %3d programs (%d alone)' % (c, k, O[c]))
        print('    paid for: ' + ', '.join('%s %d' % (k, v) for k, v in U.most_common()))


if __name__ == '__main__':
    main()
