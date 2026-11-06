#!/usr/bin/env python3
"""run.py OUT [-j N] [--match RE] [--engine PATH] [--build-only]: reference LAPACK on
SLOW-32, its own test suite, with the same built by gfortran on the host beside it.

LAPACK is BSD but stays in ~/refs (LAPACK= says where).  Copies go in OUT, each built
by LAPACK's own Makefiles: OUT/s32 through s32fc, OUT/host by gfortran.  A side is built once:
OUT/side/.built says it was, so a change of compiler wants a new OUT.  (Run on a finished
tree, where make has deleted the intermediate la_xisnan.mod, Apple's make 3.81 walks
LAPACK's dependencies for longer than anyone has waited.)  Each suite's output lands in
OUT/{s32,host}/TESTING/NAME.out.  A suite counts as LAPACK's own lapack_testing.py
counts it: tests run, tests over the threshold, illegal-value and INFO errors.

The host is built with -ffp-contract=off, the arithmetic SLOW-32 has, and the two then
print the same numbers.  Exit 0 only if every suite ran to its end on SLOW-32 and no
count is worse than the host's.  A count of tests failing the threshold is a question of how good the
arithmetic is, so it is what this measures: numerical tests with a threshold are
allowed to differ between machines, and the report shows both.
"""
import argparse, collections, os, re, shutil, subprocess, sys, time
from concurrent.futures import ThreadPoolExecutor

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
LAPACK = os.environ.get('LAPACK', os.path.expanduser('~/refs/lapack'))
PINNED = '6ec7f2bc4ecf4c4a93496aa2fa519575bc0e39ca'  # v3.12.1
# -I: the guest's own libm.  The DBT's stubs call the host's, which is allowed to differ
# from ours in the last place and with it to tip a marginal eigenproblem: ssep's order-20
# matrix fails on them and passes on musl, native flang the same.  A verdict shouldn't
# depend on the host's libm.
DBT = os.path.join(ROOT, 'tools/dbt/slow32-dbt') + ' -I'
TIMEOUT = 3000
BUILD_TIMEOUT = 1800
SHARED = ('nep', 'sep', 'se2', 'svd', 'glm', 'gqr', 'gsv', 'csd', 'lse')  # one input, each precision


def suites():
    """(name, binary directory and program, input): lapack_testing.py's list."""
    out = []
    for c in 'sdcz':
        for i in ['nep', 'sep', 'se2', 'svd', c + 'ec', c + 'ed', c + 'gg', c + 'gd', c + 'sb',
                  c + 'sg', c + 'bb', 'glm', 'gqr', 'gsv', 'csd', 'lse',
                  c + 'bal', c + 'bak', c + 'gbal', c + 'gbak']:
            out.append((i if i[0] == c and i not in SHARED else c + i, 'EIG/xeigtst' + c, i))
        out.append((c + 'test', 'LIN/xlintst' + c, c + 'test'))
        out.append((c + 'test_rfp', 'LIN/xlintstrf' + c, c + 'test_rfp'))
        out.append((c + 'dmd', 'EIG/xdmdeigtst' + c, c + 'dmd'))
    out.append(('dstest', 'LIN/xlintstds', 'dstest'))
    out.append(('zctest', 'LIN/xlintstzc', 'zctest'))
    return out


def count(path):
    run = fail = illegal = info = 0
    ended = False
    with open(path, encoding='latin-1') as f:
        for line in f:
            w = line.split()
            if 'run)' in line:
                run += int(w[w.index('run)') - 2])
            if 'out of' in line:
                fail += int(w[w.index('out') - 1])
                run += int(w[w.index('of') + 1])  # a section with failures has no "run)" line
            if 'FAILED' in line:  # the DMD programs' way of saying it
                fail += 1
            if 'illegal' in line or 'Illegal' in line:
                illegal += 1
            if ' INFO' in line:
                info += 1
            if 'End of tests' in line or 'Test completed' in line:  # the DMD programs say the latter
                ended = True
    return run, fail, illegal, info, ended


def fix_upstream(tree):
    """The complex xGEQP3RK test an uninitialized MAXC2NRM in 3.12.1; the real ones assign
    it.  What the stack holds decides KFACT, so the count of tests moves with the compiler."""
    for name, idamax in (('c', 'ISAMAX'), ('z', 'IDAMAX')):
        p = os.path.join(tree, 'SRC', name + 'geqp3rk.f')
        with open(p, newline='') as f:
            s = f.read()
        a = f'      KP1 = {idamax}( N, RWORK( 1 ), 1 )\n'
        if s.count(a) != 1 or 'MAXC2NRM = RWORK( KP1 )' in s:
            sys.exit(f'{p}: not the source fix_upstream expects')
        with open(p, 'w', newline='') as f:
            f.write(s.replace(a, a + '      MAXC2NRM = RWORK( KP1 )\n'))


def build(out, side):
    tree = os.path.join(out, side)
    stamp = os.path.join(tree, '.built')
    if os.path.exists(stamp):
        return
    if not os.path.isdir(tree):
        shutil.copytree(LAPACK, tree, ignore=shutil.ignore_patterns('.git'))
        fix_upstream(tree)
    shutil.copy(os.path.join(HERE, 'make.inc.' + ('s32' if side == 's32' else 'host')),
                os.path.join(tree, 'make.inc'))
    env = dict(os.environ, S32FC=os.path.join(HERE, 's32fc'),
               S32AR=os.path.join(ROOT, 'tools/utilities/s32-ar'))
    # One at a time, since lapacklib and tmglib both make in SRC.
    for where, target in (('.', 'blaslib'), ('.', 'lapacklib'), ('.', 'tmglib'),
                          ('TESTING/LIN', 'all'), ('TESTING/EIG', 'all')):
        p = subprocess.run(['make', '-k', '-j14', target], cwd=os.path.join(tree, where), env=env,
                           stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=BUILD_TIMEOUT)
        if p.returncode != 0:
            tail = p.stdout.decode('latin-1').strip().splitlines()[-12:]
            sys.exit(f'{side} {where}: make failed\n' + '\n'.join(tail))
    open(stamp, 'w').close()


def one(side, s, engine):
    name, prog, inp = s
    t = os.path.join(side, 'TESTING')
    src = os.path.join(t, inp + '.in')
    if not os.path.exists(src):
        return None
    outpath = os.path.join(t, name + '.out')
    cmd = engine.split() + [prog] if engine else ['./' + prog]
    start = time.time()
    try:
        with open(src) as i, open(outpath, 'w') as o:
            rc = subprocess.run(cmd, cwd=t, stdin=i, stdout=o, stderr=subprocess.STDOUT, timeout=TIMEOUT).returncode
    except subprocess.TimeoutExpired:
        rc = None
    r = (rc, time.time() - start) + count(outpath)
    print(f'  {side.split(os.sep)[-1]} {name}: rc={rc} {r[1]:.1f}s', file=sys.stderr, flush=True)
    return r


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('out')
    ap.add_argument('-j', type=int, default=10)
    ap.add_argument('--match')
    ap.add_argument('--engine', default=DBT)
    ap.add_argument('--build-only', action='store_true')
    a = ap.parse_args()
    out = os.path.abspath(a.out)
    have = subprocess.run(['git', '-C', LAPACK, 'rev-parse', 'HEAD'], capture_output=True, text=True).stdout.strip()
    if have != PINNED:
        print(f'run.py: LAPACK at {LAPACK} is at {have or "nothing"}, not {PINNED}', file=sys.stderr)
        return 2
    os.makedirs(out, exist_ok=True)
    build(out, 's32')
    build(out, 'host')
    if a.build_only:
        return 0
    list_ = [s for s in suites() if not a.match or re.search(a.match, s[0])]
    with ThreadPoolExecutor(a.j) as pool:
        s32 = list(pool.map(lambda s: one(os.path.join(out, 's32'), s, a.engine), list_))
        host = list(pool.map(lambda s: one(os.path.join(out, 'host'), s, None), list_))
    bad = 0
    totals = collections.Counter()
    print(f'{"suite":14} {"tests":>9} {"fail s32":>8} {"fail host":>9} {"illegal":>8} {"info":>6} {"s32 s":>7} {"host s":>7}')
    with open(os.path.join(out, 'results.tsv'), 'w') as tsv:
        for s, r, h in zip(list_, s32, host):
            if r is None:
                continue
            note = ''
            if r[0] != 0 or not r[6]:
                note = f'  DID NOT FINISH rc={r[0]}'
                bad += 1
            elif h and (r[3] > h[3] or r[4] > h[4] or r[5] > h[5] or r[2] != h[2]):
                note = '  WORSE'
                bad += 1
            print(f'{s[0]:14} {r[2]:9} {r[3]:8} {h[3] if h else -1:9} {r[4]:8} {r[5]:6} {r[1]:7.1f} {h[1] if h else 0:7.1f}{note}', flush=True)
            tsv.write('\t'.join(map(str, (s[0], r[0], r[2], r[3], r[4], r[5], h[2], h[3], h[4], h[5]))) + '\n')
            totals['tests'] += r[2]
            totals['fail'] += r[3]
            totals['hostfail'] += h[3]
            totals['illegal'] += r[4]
            totals['info'] += r[5]
    print(f'{totals["tests"]} tests; failing the threshold: {totals["fail"]} on SLOW-32, {totals["hostfail"]} on the host; '
          f'illegal {totals["illegal"]}, info {totals["info"]}; {bad} suites worse or unfinished')
    return 1 if bad else 0


if __name__ == '__main__':
    sys.exit(main())
