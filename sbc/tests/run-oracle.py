#!/usr/bin/env python3
"""The interpreter is the oracle: every program is run by sbasic.s32x and
compiled by s32-sbc, with the same keys on stdin, and the two must print
the same bytes.

    sbc/tests/run-oracle.py [-j N] [-v] [--bc] [DIR_OR_FILE ...]

With no arguments the corpus is sbasic's own tests, s32-bc's native
paths' (tests/native: each err-* program ends in the run-time error it
is named for) and, when they are
there, the collections in ~/refs/basic/nbs-ecma55-test (NBS/,
basic_computer_games/, fast/; not in this tree).  A program's keys are
its sibling .in or .input.

Verdicts: AGREE; DIFFER (a gate failure: both outputs and a diff are
kept in out/oracle/); NOCOMPILE (s32-sbc refuses it: where the
interpreter refuses it too, at the same line, that is agreement, and
counted as AGREE-REJECT); TIMEOUT (either side).  Exit status 1 when
anything DIFFERs.

--kit bc and --kit sbc do it with the compiler, its run time and every tool
self-hosted (docker/bin/s32kbasic, KIT= the kit; tests/run-kit.sh).

--bc holds s32-bc (bc/, compile-bc.sh) to the interpreter instead.  Its
refusals are tallied by reason: a checker finding (the program is no
QuickBASIC loads; the interpreter runs it anyway), or "not yet: ..." --
what s32-bc does not do yet, the list of what to do next.
"""
import concurrent.futures as cf
import difflib
import glob
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
SBC = os.path.dirname(HERE)
ROOT = os.path.dirname(SBC)
EMU = os.environ.get('EMU', os.path.join(ROOT, 'tools/emulator/slow32-fast'))
SBASIC = os.path.join(ROOT, 'sbasic/sbasic.s32x')
OUT = os.path.join(SBC, 'out/oracle')
COMPILE = 'compile-bas.sh'
KIT_CMD = None      # --kit bc|sbc: the self-hosted kit's driver (KIT= the kit, run-kit.sh)
BUILD = 'build.sh' 
REFS = os.path.expanduser('~/refs/basic/nbs-ecma55-test')
TLIMIT = int(os.environ.get('TIMEOUT', '60'))


def keys_for(path):
    base = os.path.splitext(path)[0]
    for ext in ('.in', '.input'):
        if os.path.exists(base + ext):
            return open(base + ext, 'rb').read()
    return b''


def run(argv, keys, cwd):
    try:
        r = subprocess.run(argv, input=keys, capture_output=True, timeout=TLIMIT, cwd=cwd)
    except subprocess.TimeoutExpired:
        return None
    return r.stdout


def one(path):
    name = os.path.relpath(path, ROOT) if path.startswith(ROOT) else path
    tag = name.replace('/', '_')
    keys = keys_for(path)
    # where a program runs (and is compiled: $INCLUDE paths are relative):
    # its directory, but sbasic's tests from sbasic/, as run-tests.sh does
    cwd = os.path.dirname(path)
    if os.path.basename(cwd) == 'tests' and os.path.basename(os.path.dirname(cwd)) == 'sbasic':
        cwd = os.path.dirname(cwd)
    exe = os.path.join(OUT, tag + '.s32x')
    work = os.path.join(OUT, 'work-' + tag)
    os.makedirs(work, exist_ok=True)
    c = subprocess.run(KIT_CMD + [path, exe] if KIT_CMD else ['bash', os.path.join(SBC, COMPILE), path, exe],
                       capture_output=True, cwd=cwd, env=dict(os.environ, SBC_WORK=work))
    interp = run([EMU, '-q', SBASIC, path], keys, cwd)
    if interp is not None:
        # the interpreter's "Loaded N lines from F" (a compiled program
        # has no loading to report)
        lines = interp.split(b'\n')
        for i, l in enumerate(lines[:3]):
            if l.startswith(b'Loaded ') and b' lines from ' in l:
                del lines[i]
                break
        interp = b'\n'.join(lines)
    if c.returncode != 0:
        msg = c.stderr.decode('latin-1').strip().split('\n')[-1]
        # the interpreter refuses it too: a parse error, or the same error
        # before running a line (FOR without NEXT is found by lowering)
        if interp is not None and (interp.startswith(b'Error in line') or
                                   interp.strip() == ('Error: ' + msg.split(': ')[-1]).encode()):
            return name, 'AGREE-REJECT', msg
        return name, 'NOCOMPILE', msg
    comp = run([EMU, '-q', exe], keys, cwd)
    if interp is None or comp is None:
        return name, 'TIMEOUT', 'interpreter' if interp is None else 'compiled'
    if interp == comp:
        return name, 'AGREE', ''
    open(os.path.join(OUT, tag + '.interp'), 'wb').write(interp)
    open(os.path.join(OUT, tag + '.comp'), 'wb').write(comp)
    d = difflib.unified_diff(interp.decode('latin-1').split('\n'), comp.decode('latin-1').split('\n'),
                             'interpreter', 'compiled', lineterm='', n=1)
    open(os.path.join(OUT, tag + '.diff'), 'w').write('\n'.join(d) + '\n')
    return name, 'DIFFER', ''


def corpus(args):
    paths = []
    for a in args:
        a = os.path.abspath(os.path.expanduser(a))
        if os.path.isdir(a):
            paths += sorted(p for p in glob.glob(os.path.join(a, '*')) if p.lower().endswith('.bas'))
        else:
            paths.append(a)
    return paths


def main():
    args = sys.argv[1:]
    jobs, verbose = 8, '-v' in args
    if '-j' in args:
        jobs = int(args[args.index('-j') + 1])
        del args[args.index('-j'):args.index('-j') + 2]
    args = [a for a in args if a != '-v']
    global OUT, COMPILE, BUILD, KIT_CMD
    kit = None
    if '--kit' in args:
        kit = args[args.index('--kit') + 1]
        del args[args.index('--kit'):args.index('--kit') + 2]
        KIT_CMD = [os.path.join(ROOT, 'docker/bin/s32kbasic'), {'bc': '--qb', 'sbc': '--any'}[kit]]
        OUT = os.path.join(SBC, 'out/oracle-kit-' + kit)
    bc = '--bc' in args or kit == 'bc'
    if bc:
        args = [a for a in args if a != '--bc']
        OUT, COMPILE, BUILD = kit and OUT or os.path.join(SBC, 'out/oracle-bc'), 'compile-bc.sh', 'build-bc.sh'
    if not args:
        args = [os.path.join(ROOT, 'sbasic/tests'), os.path.join(HERE, 'native')]
        args += [os.path.join(REFS, d) for d in ('NBS', 'basic_computer_games', 'fast')
                 if os.path.isdir(os.path.join(REFS, d))]
    os.makedirs(OUT, exist_ok=True)
    # the runtime's own object, made once before the programs race for it
    if not kit:
        subprocess.run(['bash', os.path.join(SBC, BUILD)], check=True, capture_output=True)
    with cf.ThreadPoolExecutor(max_workers=jobs) as ex:
        res = list(ex.map(one, corpus(args)))
    counts = {}
    for name, verdict, note in res:
        counts[verdict] = counts.get(verdict, 0) + 1
        if verbose or verdict not in ('AGREE', 'AGREE-REJECT'):
            print('%-60s %s %s' % (name, verdict, note))
    print('  '.join('%s %d' % (k, counts[k]) for k in sorted(counts)), '(of %d)' % len(res))
    if bc:
        why = {}
        for name, verdict, note in res:
            if verdict in ('NOCOMPILE', 'AGREE-REJECT'):
                k = note[note.index('not yet'):] if 'not yet' in note else \
                    note.split(': ')[2] if note.count(': ') >= 2 else note
                why[k] = why.get(k, 0) + 1
        for k in sorted(why, key=lambda k: -why[k]):
            print('    %4d  %s' % (why[k], k))
    sys.exit(1 if counts.get('DIFFER') else 0)


if __name__ == '__main__':
    main()
