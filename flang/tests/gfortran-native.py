#!/usr/bin/env python3
"""gfortran-native.py OUT [-j N]: the corpus through native flang, from the same
sources, and each verdict against flang/tests/gfortran-verdicts.txt.

A test that fails on SLOW-32 and passes natively is ours, whatever LLVM's lists say
of it; one that passes natively and on SLOW-32 and differed before a patch says the
patch reached 64 bits.  NATIVE_FLANG= says which flang (default build-flang-native,
built by docs/plans/flang.md's recipe).  OUT must not exist; OUT/native.tsv has every
verdict, OUT/ours.txt the tests that fail only on SLOW-32.  Not a gate: a report.
"""
import argparse, collections, importlib.util, os, subprocess, sys
from concurrent.futures import ThreadPoolExecutor

HERE = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location('suite', os.path.join(HERE, 'gfortran-suite.py'))
suite = importlib.util.module_from_spec(spec)
spec.loader.exec_module(suite)

FLANG = os.environ.get('NATIVE_FLANG', os.path.expanduser('~/llvm-project/build-flang-native/bin/flang'))
FLANG_HEADERS = os.path.expanduser('~/llvm-project/flang/include/flang')
CLANG, SDK = 'clang', subprocess.run(['xcrun', '--show-sdk-path'], capture_output=True, text=True).stdout.strip()
AGREE = suite.AGREE


def native(rel, out):
    src = os.path.join(suite.SUITE, rel)
    with open(src, encoding='latin-1') as f:
        text = f.read()
    _, opts, shouldfail = suite.directives(text)
    work = os.path.join(out, 'w', rel.replace('/', '__'))
    os.makedirs(work)
    aux, more = suite.companions(src, text)
    for m in aux:
        suite.proc([suite.FC, '-w', '-O1', '-c'] + opts + [m], work, suite.BUILD_TIMEOUT)
    rc, _, _ = suite.proc([suite.FC, '-w', '-O1'] + opts + [src] + more + ['-o', 'h'], work, suite.BUILD_TIMEOUT)
    if rc != 0:
        return rel, 'oracle-refuses', ''
    hrc, hout, _ = suite.proc(['./h'], work, suite.HOST_TIMEOUT)
    os.unlink(os.path.join(work, 'h'))
    if hrc is None or (hrc != 0) != shouldfail:
        return rel, 'oracle-fails', ''
    flags = [o for o in opts if suite.PASS_ON.match(o)]
    for m in aux:
        suite.proc([FLANG, '-w', '-O2', '-c'] + flags + [m, '-o', 'aux.o'], work, suite.BUILD_TIMEOUT)
    linked = []
    for i, m in enumerate(more):  # flang's driver compiles no C
        if m.endswith('.c'):
            suite.proc([CLANG, '-w', '-O2', '-I', FLANG_HEADERS, '-isysroot', SDK] + suite.C_FOR_FLANG + ['-c', m, '-o', f'c{i}.o'], work, suite.BUILD_TIMEOUT)
            linked.append(f'c{i}.o')
        else:
            linked.append(m)
    rc, _, err = suite.proc([FLANG, '-w', '-O2'] + flags + [src] + linked + ['-o', 'n'], work, suite.BUILD_TIMEOUT)
    if rc is None:
        return rel, 'build-timeout', ''
    if rc != 0:
        return rel, 'flang', suite.first_error(err)
    trc, tout, terr = suite.proc(['./n'], work, suite.RUN_TIMEOUT)
    os.unlink(os.path.join(work, 'n'))
    if trc is None:
        return rel, 'timeout', ''
    if (trc != 0) != (hrc != 0):
        return rel, 'exit-differs', f'rc={trc} {suite.first_error(terr)[:100]}'
    if tout != hout:
        return rel, ('format-only' if suite.same_tokens(hout, tout) else 'text-differs'), ''
    return rel, 'pass', ''


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('out')
    ap.add_argument('-j', type=int, default=8)
    a = ap.parse_args()
    out = os.path.abspath(a.out)
    os.makedirs(out)
    ours = {}
    with open(os.path.join(HERE, 'gfortran-verdicts.txt')) as f:
        for line in f:
            if not line.startswith('#'):
                v, t = line.rstrip('\n').split('\t')
                ours[t] = v
    judged = sorted(t for t, v in ours.items() if not v.startswith(('skip-', 'oracle-')))
    results = []
    with ThreadPoolExecutor(a.j) as pool, open(os.path.join(out, 'native.tsv'), 'w') as tsv:
        for r in pool.map(lambda t: native(t, out), judged):
            results.append(r)
            tsv.write('\t'.join(r) + '\n')
            tsv.flush()
    table = collections.Counter()
    only_ours = []
    for t, v, note in results:
        n = 'agree' if v in AGREE else v
        s = 'agree' if ours[t] in AGREE else 'fails'
        table[(s, 'agree' if n == 'agree' else ('oracle' if n.startswith('oracle') else 'fails'))] += 1
        if s == 'fails' and n == 'agree':
            only_ours.append(f'{ours[t]}\t{t}')
    print(f'{len(results)} tests with a verdict on SLOW-32')
    print(f'  both agree with gfortran            {table[("agree", "agree")]}')
    print(f'  both fail                           {table[("fails", "fails")]}')
    print(f'  SLOW-32 fails, native agrees        {table[("fails", "agree")]}   <- ours')
    print(f'  SLOW-32 agrees, native fails        {table[("agree", "fails")]}')
    print(f'  no oracle this time                 {table[("agree", "oracle")] + table[("fails", "oracle")]}')
    with open(os.path.join(out, 'ours.txt'), 'w') as f:
        f.write('\n'.join(sorted(only_ours)) + '\n')
    print(f'ours: {os.path.join(out, "ours.txt")}')


if __name__ == '__main__':
    sys.exit(main())
