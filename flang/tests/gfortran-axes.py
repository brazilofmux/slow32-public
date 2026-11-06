#!/usr/bin/env python3
"""gfortran-axes.py OUT [-j N] [--match RE] [--sample K]: the corpus along the axes
the gate does not vary, no oracle needed: each program must do the same thing on
every engine, and LLVM's machine verifier must accept every function.

The gate (gfortran-suite.py) builds one way and runs on the DBT.  Here each test
gfortran-verdicts.txt says flang compiles is built, llc run again with
-verify-machineinstrs (it must pass and emit the same assembly), and the program run
on slow32, slow32-fast, slow32-dbt and qemu, each in its own directory, since the
tests write files.  Exit code and output must match the reference interpreter.  An
engine out of time is counted, not a difference: the interpreter is ~30x the DBT.

--variants instead builds each at other optimization levels (the verifier on for
all) and links it with the self-hosted s32-ld, and each must behave as the plain build
does on the DBT.  stage08's s32-as cannot read LLVM's assembly, so it is not an axis.

OUT must not exist; OUT/results.tsv has every test, OUT/w/TEST what each engine did
for the tests that differ.  Not a gate: a report.  QEMU_S32= says which qemu.
"""
import argparse, collections, os, re, shutil, sys
from concurrent.futures import ThreadPoolExecutor
import importlib.util

HERE = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location('suite', os.path.join(HERE, 'gfortran-suite.py'))
suite = importlib.util.module_from_spec(spec)
spec.loader.exec_module(suite)

ROOT = suite.ROOT
LLC = os.path.join(os.environ.get('LLVM_PROJECT', os.path.expanduser('~/llvm-project')), 'build-flang/bin/llc')
QEMU = os.environ.get('QEMU_S32', os.path.expanduser('~/qemu/build/qemu-system-slow32'))
ENGINES = {
    'slow32': [os.path.join(ROOT, 'tools/emulator/slow32'), '-q'],
    'fast': [os.path.join(ROOT, 'tools/emulator/slow32-fast'), '-q'],
    'dbt': [suite.DBT],
    'qemu': [QEMU, '-machine', 'slow32-tcg', '-nographic', '-monitor', 'none', '-kernel'],
}
REFERENCE = 'slow32'
TIMEOUT = 180
BUILT = ('pass', 'format-only', 'text-differs', 'exit-differs', 'timeout')
# run-differential.sh's normalize_output: the same fault in each engine's words, and
# what an engine says of itself gone.
FAULTS = [(re.compile(p), r) for p, r in (
    (r'^Memory fault: Failed to .* bytes at (0x[0-9A-Fa-f]*).*', 'FAULT addr='),
    (r'^Error: .* out of bounds( or to protected memory)? at (0x[0-9A-Fa-f]*).*', 'FAULT addr='),
    (r'^DBT: Memory fault at PC=[0-9xA-Fa-f]*, addr=(0x[0-9A-Fa-f]*).*', 'FAULT addr='),
    (r'^Unknown opcode: 0x[0-9A-Fa-f]* at PC=(0x[0-9A-Fa-f]*).*', 'ILLEGAL pc='),
    (r'^Invalid instruction at PC (0x[0-9A-Fa-f]*).*', 'ILLEGAL pc='),
    (r'^DBT: Illegal instruction at PC=(0x[0-9A-Fa-f]*).*', 'ILLEGAL pc='),
)]
OWN = re.compile(r'^(Starting execution|MMIO enabled|HALT at|Program halted|Exit code:|Instructions executed'
                 r'|Simulated cycles:|Wall time:|Performance:|Cycles:|.*instructions/second|slow32: native intrinsics'
                 r'|slow32: math intercepts:|  mem[a-z]*: +0x[0-9a-f]*$|  strlen: +0x[0-9a-f]*$)')


def guest(text):
    """What the program said on one stream, faults canonical."""
    lines = []
    for line in text.decode('latin-1').replace('\r\n', '\n').split('\n'):
        for pat, tag in FAULTS:
            m = pat.match(line)
            if m:
                line = tag + m.group(m.lastindex).lower()
                break
        if not OWN.match(line):
            lines.append(line)
    while lines and not lines[-1].strip():
        lines.pop()
    return lines


def verify(work):
    """None, or what the verifier said, or that it changed the code."""
    rc, _, err = suite.proc([LLC, '-mtriple=slow32-unknown-none', '-O2', '-verify-machineinstrs',
                             't.ll', '-o', 'v.s'], work, suite.BUILD_TIMEOUT)
    if rc != 0:
        return 'verifier', suite.first_error(err)[:200] if rc is not None else 'timeout'
    with open(os.path.join(work, 't.s'), 'rb') as a, open(os.path.join(work, 'v.s'), 'rb') as b:
        if a.read() != b.read():
            return 'verify-changed-code', ''
    return None


def one(rel, verdict, out):
    src = os.path.join(suite.SUITE, rel)
    with open(src, encoding='latin-1') as f:
        _, opts, _ = suite.directives(f.read())
    work = os.path.join(out, 'w', rel.replace('/', '__'))
    os.makedirs(work)
    env = dict(os.environ, FFLAGS=' '.join(o for o in opts if suite.PASS_ON.match(o)))
    rc, _, err = suite.proc(['zsh', suite.DRIVER, src, os.path.join(work, 't.s32x')], work, suite.BUILD_TIMEOUT, env)
    if rc != 0:
        return rel, 'build', f'rc={rc} {suite.first_error(err)[:150]}', {}
    bad = verify(work)
    if bad:
        return rel, bad[0], bad[1], {}

    ran = {}
    for name, cmd in ENGINES.items():
        d = os.path.join(work, name)
        os.makedirs(d)
        ran[name] = suite.proc(cmd + ['../t.s32x'], d, TIMEOUT)
    ref = ran[REFERENCE]
    late = [n for n, r in ran.items() if r[0] is None]
    differ = []
    for name, r in ran.items():
        if name in late or REFERENCE in late or name == REFERENCE:
            continue
        if r[0] != ref[0]:
            differ.append(f'{name}:rc {r[0]} not {ref[0]}')
        elif guest(r[1]) + guest(r[2]) != guest(ref[1]) + guest(ref[2]):
            differ.append(f'{name}:output')
    if differ:
        for name, r in ran.items():
            with open(os.path.join(work, name + '.out'), 'wb') as f:
                f.write(r[1])
            with open(os.path.join(work, name + '.err'), 'wb') as f:
                f.write(r[2])
        for n in ('t.s32o', 'v.s'):
            os.unlink(os.path.join(work, n))
        return rel, 'differ', ' '.join(differ), ref
    shutil.rmtree(work)
    if late:
        return rel, 'late', ' '.join(late), {}
    return rel, 'same', '', {}


# name: what s32flang is told.  Each must behave as the plain build does, on the DBT.
VARIANTS = {
    'fc0-llc0': {'FC_OPT': '-O0', 'LLC_OPT': '-O0'},
    'fc1-llc1': {'FC_OPT': '-O1', 'LLC_OPT': '-O1'},
    'fc0-llc2': {'FC_OPT': '-O0'},
    'fc2-llc0': {'LLC_OPT': '-O0'},
    'fc3-llc3': {'FC_OPT': '-O3', 'LLC_OPT': '-O3'},
    'selfld': {'S32_LD': suite.DBT + ' ' + os.path.join(ROOT, 'selfhost/stage08/s32-ld.s32x')},
}
VARIANT_TIMEOUT = 120


def behaviour(work, name, src, base):
    """(rc, output) of src built with base + VARIANTS[name], or a phase's refusal."""
    d = os.path.join(work, name)
    os.makedirs(os.path.join(d, 'run'))
    env = dict(base, LLCFLAGS='-verify-machineinstrs', **VARIANTS.get(name, {}))
    rc, _, err = suite.proc(['zsh', suite.DRIVER, src, os.path.join(d, 't.s32x')], d, suite.BUILD_TIMEOUT, env)
    if rc != 0:
        return f'{suite.PHASE.get(rc, rc)}: {suite.first_error(err)[:150]}', None, b'', b''
    r = suite.proc([suite.DBT, '../t.s32x'], os.path.join(d, 'run'), VARIANT_TIMEOUT)
    return None, r[0], r[1], r[2]


def variants(rel, verdict, out):
    src = os.path.join(suite.SUITE, rel)
    with open(src, encoding='latin-1') as f:
        _, opts, _ = suite.directives(f.read())
    work = os.path.join(out, 'w', rel.replace('/', '__'))
    os.makedirs(work)
    env = dict(os.environ, FFLAGS=' '.join(o for o in opts if suite.PASS_ON.match(o)))
    refused, brc, bout, berr = behaviour(work, 'plain', src, env)
    if refused:
        return rel, 'build', refused, {}
    if brc is None:
        shutil.rmtree(work)
        return rel, 'late', 'plain', {}
    notes, late = [], []
    for name in VARIANTS:
        refused, rc, o, e = behaviour(work, name, src, env)
        if refused:
            notes.append(f'{name}:{refused}')
        elif rc is None:
            late.append(name)
        elif rc != brc:
            notes.append(f'{name}:rc {rc} not {brc}')
        elif guest(o) + guest(e) != guest(bout) + guest(berr):
            notes.append(f'{name}:output')
        for ext, data in (('.out', o), ('.err', e)):
            with open(os.path.join(work, name + ext), 'wb') as f:
                f.write(data)
    if notes:
        for ext, data in (('.out', bout), ('.err', berr)):
            with open(os.path.join(work, 'plain' + ext), 'wb') as f:
                f.write(data)
        return rel, 'differ', ' | '.join(notes), {}
    shutil.rmtree(work)
    return rel, ('late' if late else 'same'), ' '.join(late), {}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('out')
    ap.add_argument('-j', type=int, default=12)
    ap.add_argument('--match')
    ap.add_argument('--sample', type=int)
    ap.add_argument('--variants', nargs='?', const=','.join(VARIANTS), help='optimization levels and the self-hosted linker; or these, comma-separated')
    a = ap.parse_args()
    out = os.path.abspath(a.out)
    chosen = a.variants.split(',') if a.variants else []
    for name in set(VARIANTS) - set(chosen):
        del VARIANTS[name]
    for cmd in ENGINES.values():
        if not os.access(cmd[0], os.X_OK):
            sys.exit(f'no engine {cmd[0]}')
    os.makedirs(out)
    tests = []
    with open(os.path.join(HERE, 'gfortran-verdicts.txt')) as f:
        for line in f:
            if not line.startswith('#'):
                v, t = line.rstrip('\n').split('\t')
                if v in BUILT and (not a.match or re.search(a.match, t)):
                    tests.append((t, v))
    if a.sample:
        tests = tests[::a.sample]
    counts = collections.Counter()
    with ThreadPoolExecutor(a.j) as pool, open(os.path.join(out, 'results.tsv'), 'w') as tsv:
        for t, kind, note, _ in pool.map(lambda x: (variants if a.variants else one)(x[0], x[1], out), tests):
            counts[kind] += 1
            tsv.write(f'{kind}\t{t}\t{note}\n')
            tsv.flush()
            if kind not in ('same', 'late'):
                print(f'{kind:10} {t}  {note[:110]}', flush=True)
    print(f'{len(tests)} tests: ' + ', '.join(f'{k} {n}' for k, n in counts.most_common()))


if __name__ == '__main__':
    main()
