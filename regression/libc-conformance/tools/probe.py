#!/usr/bin/env python3
"""Clause 7 behaviour probes: both SLOW-32 C libraries against two host ones.

usage: probe.py [-v] [--check | --update] [probe.c ...]   (default: every probe)

Each probe is built and run four ways:
  rt     clang for slow32 + runtime/ (libc_mmio), on the reference emulator
  sh     stage08 cc + the selfhost library, on the reference emulator
  mac    the host's cc and libc
  glibc  gcc and glibc in a Linux container (podman, docker.io/library/gcc),
         all probes in one container run; results cached by content
The expected output is what mac and glibc both print.  Where they differ,
a <probe>.expect beside the probe is the ruling; without one the probe is
ORACLE-SPLIT.  Output = stdout, then a line "[exit N]".  Every leg runs
with TZ=America/Chicago and LC_ALL=C, stdin from <probe>.in or /dev/null,
in a scratch directory of its own.

Verdict per library: PASS, DIFFER, NOBUILD (it would not compile or link:
usually an identifier the library lacks), TIMEOUT.  verdicts.txt holds
"name rt sh" per probe; --check fails when a PASS became anything else
and names probes that got better; --update rewrites it.
"""
import concurrent.futures as cf, hashlib, os, re, shutil, subprocess, sys, tempfile

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ROOT = os.path.abspath(os.path.join(HERE, '../..'))
LLVM = os.environ.get('LLVM_BIN', os.path.expanduser('~/llvm-project/build/bin'))
AS = ROOT + '/tools/assembler/slow32asm'
LD = ROOT + '/tools/linker/s32-ld'
DBT = ROOT + '/tools/dbt/slow32-dbt'
REF = ROOT + '/tools/emulator/slow32'
CC8 = ROOT + '/selfhost/stage08/cc.s32x'
L8 = ROOT + '/selfhost/stage08/lib'
CACHE = os.path.expanduser('~/.cache/slow32-libc-conformance')
ENV = dict(os.environ, TZ='America/Chicago', LC_ALL='C')
TLIMIT = int(os.environ.get('PROBE_TIMEOUT', '60'))
IMAGE = os.environ.get('GLIBC_IMAGE', 'docker.io/library/gcc:latest')

def sh(cmd, cwd=None, inp=None, timeout=600):
    try:
        r = subprocess.run(cmd, cwd=cwd, env=ENV, stdin=open(inp) if inp else subprocess.DEVNULL,
                           stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, timeout=timeout)
        return r.returncode, r.stdout
    except subprocess.TimeoutExpired:
        return None, b''

def result(code, out):
    if code is None:
        return None
    return out.replace(b'\r\n', b'\n') + b'[exit %d]\n' % code

def run_emu(exe, d, inp):
    code, out = sh([REF, '-q', exe], cwd=d, inp=inp, timeout=TLIMIT)
    if code is None:
        return None
    out = b''.join(l for l in out.splitlines(True) if not l.startswith(b'HALT'))
    return result(code, out)

def leg_rt(src, d, inp):
    ok = sh([LLVM + '/clang', '-target', 'slow32-unknown-none', '-std=c11', '-w', '-O1', '-S', '-emit-llvm',
             '-I' + ROOT + '/runtime/include', src, '-o', d + '/c.ll'])[0] == 0 and \
         sh([LLVM + '/llc', '-mtriple=slow32-unknown-none', d + '/c.ll', '-o', d + '/c.s'])[0] == 0 and \
         sh([AS, d + '/c.s', d + '/c.s32o'])[0] == 0 and \
         sh([LD, '-o', d + '/c.s32x', '--mmio', '64K', '--heap-size', '16M', ROOT + '/runtime/crt0.s32o',
             d + '/c.s32o', ROOT + '/runtime/libc_mmio.s32a', ROOT + '/runtime/libs32.s32a'])[0] == 0
    return run_emu(d + '/c.s32x', d, inp) if ok else 'NOBUILD'

def leg_sh(src, d, inp):
    ok = sh([DBT, CC8, '-I' + ROOT + '/selfhost/stage08/include', src, d + '/s.s'])[0] == 0 and \
         os.path.exists(d + '/s.s') and \
         sh([AS, d + '/s.s', d + '/s.s32o'])[0] == 0 and \
         sh([LD, '-o', d + '/s.s32x', '--mmio', '64K', '--heap-size', '16M', L8 + '/crt0.s32o',
             d + '/s.s32o', SELF_LIBC])[0] == 0
    return run_emu(d + '/s.s32x', d, inp) if ok else 'NOBUILD'

def leg_mac(src, d, inp):
    if sh(['cc', '-std=c11', '-w', '-O1', src, '-o', d + '/h', '-lm'])[0] != 0:
        return 'NOBUILD'
    return result(*sh([d + '/h'], cwd=d, inp=inp, timeout=TLIMIT))

def key(src):
    h = hashlib.sha256(open(src, 'rb').read())
    if os.path.exists(stdin_of(src)):
        h.update(open(stdin_of(src), 'rb').read())
    return h.hexdigest()

def stdin_of(src):
    return src[:-2] + '.in'

def glibc_results(srcs):
    """build and run every probe not yet cached, in one container"""
    os.makedirs(CACHE, exist_ok=True)
    todo = [s for s in srcs if not os.path.exists(os.path.join(CACHE, key(s)))]
    if todo:
        w = tempfile.mkdtemp(dir=ROOT + '/regression')   # the repo root is what podman's VM sees
        try:
            for i, s in enumerate(todo):
                shutil.copy(s, '%s/p%d.c' % (w, i))
                if os.path.exists(stdin_of(s)):
                    shutil.copy(stdin_of(s), '%s/p%d.in' % (w, i))
            script = ('cd /r/' + os.path.relpath(w, ROOT) + ' && for c in p*.c; do b=${c%.c}; mkdir -p run_$b; '
                      'if gcc -std=c11 -w -O1 $c -o run_$b/x -lm 2>/dev/null; then '
                      'in=/dev/null; [ -f $b.in ] && in=$PWD/$b.in; '
                      '(cd run_$b && TZ=America/Chicago LC_ALL=C timeout @TLIMIT@ ./x < $in > ../$b.out 2>/dev/null; '
                      'echo "[exit $?]" >> ../$b.out); else echo NOBUILD > $b.out; fi; done').replace('@TLIMIT@', str(TLIMIT))
            subprocess.run(['podman', 'run', '--rm', '-v', ROOT + ':/r', IMAGE, 'bash', '-c', script],
                           stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=3600)
            for i, s in enumerate(todo):
                p = '%s/p%d.out' % (w, i)
                if os.path.exists(p):
                    shutil.copy(p, os.path.join(CACHE, key(s)))
        finally:
            shutil.rmtree(w, ignore_errors=True)
    res = {}
    for s in srcs:
        p = os.path.join(CACHE, key(s))
        if not os.path.exists(p):
            res[s] = 'NORUN'
            continue
        b = open(p, 'rb').read()
        res[s] = 'NOBUILD' if b == b'NOBUILD\n' else (None if b.endswith(b'[exit 124]\n') else b)
    return res

def one(src, glibc):
    inp = stdin_of(src) if os.path.exists(stdin_of(src)) else None
    legs = {}
    for name, fn in (('rt', leg_rt), ('sh', leg_sh), ('mac', leg_mac)):
        with tempfile.TemporaryDirectory() as d:
            legs[name] = fn(src, d, inp)
    legs['glibc'] = glibc
    exp_file = src[:-2] + '.expect'
    if os.path.exists(exp_file):
        expected = open(exp_file, 'rb').read()
    elif legs['mac'] == legs['glibc'] and isinstance(legs['mac'], bytes):
        expected = legs['mac']
    else:
        expected = None
    v = {}
    for lib in ('rt', 'sh'):
        r = legs[lib]
        if r == 'NOBUILD':
            v[lib] = 'NOBUILD'
        elif r is None:
            v[lib] = 'TIMEOUT'
        elif expected is None:
            v[lib] = 'ORACLE-SPLIT'
        else:
            v[lib] = 'PASS' if r == expected else 'DIFFER'
    return v, legs, expected

def show(b):
    if isinstance(b, bytes):
        return b.decode('utf-8', 'replace')
    return '<%s>\n' % ('TIMEOUT' if b is None else b)

def main():
    global SELF_LIBC
    args = sys.argv[1:]
    verbose = '-v' in args
    mode = 'check' if '--check' in args else 'update' if '--update' in args else None
    srcs = [os.path.abspath(a) for a in args if a.endswith('.c')]
    if not srcs:
        pd = HERE + '/probes'
        srcs = sorted(os.path.join(pd, f) for f in os.listdir(pd) if f.endswith('.c'))
    d = tempfile.mkdtemp()
    SELF_LIBC = d + '/libc.s32a'
    members = [L8 + '/' + f for f in sorted(os.listdir(L8)) if f.endswith('.s32o') and f != 'crt0.s32o']
    subprocess.run([ROOT + '/tools/utilities/s32-ar', 'rc', SELF_LIBC] + members, stdout=subprocess.DEVNULL)
    g = glibc_results(srcs)
    with cf.ThreadPoolExecutor(max_workers=int(os.environ.get('JOBS', '10'))) as ex:
        futs = {s: ex.submit(one, s, g[s]) for s in srcs}
        res = {s: f.result() for s, f in futs.items()}
    shutil.rmtree(d, ignore_errors=True)
    counts = {}
    lines = []
    for s in srcs:
        v, legs, expected = res[s]
        name = os.path.basename(s)[:-2]
        lines.append('%s %s %s' % (name, v['rt'], v['sh']))
        for lib in ('rt', 'sh'):
            counts[(lib, v[lib])] = counts.get((lib, v[lib]), 0) + 1
        print('%-48s rt=%-12s sh=%s' % (name, v['rt'], v['sh']))
        if verbose and (v['rt'] != 'PASS' or v['sh'] != 'PASS'):
            if expected is None:
                print('  -- mac:\n' + show(legs['mac']) + '  -- glibc:\n' + show(legs['glibc']), end='')
            else:
                print('  -- expected:\n' + show(expected), end='')
            for lib in ('rt', 'sh'):
                if v[lib] != 'PASS':
                    print('  -- %s:\n' % lib + show(legs[lib]), end='')
    for lib in ('rt', 'sh'):
        print('%s: %s' % (lib, '  '.join('%d %s' % (n, k[1]) for k, n in sorted(counts.items()) if k[0] == lib)))
    vf = HERE + '/verdicts.txt'
    if mode == 'update':
        old = {}
        if os.path.exists(vf):
            for l in open(vf):
                p = l.split()
                if len(p) == 3: old[p[0]] = l
        for l in lines:
            old[l.split()[0]] = l + '\n'
        open(vf, 'w').write(''.join(old[k] for k in sorted(old)))
        print('verdicts.txt: %d probes' % len(old))
    if mode == 'check':
        rec = {}
        for l in open(vf):
            p = l.split()
            if len(p) == 3: rec[p[0]] = p[1:]
        worse = better = 0
        for l in lines:
            n, a, b = l.split()
            if n not in rec:
                print('NEW (not in verdicts.txt): %s' % n); worse += 1; continue
            for lib, now, was in (('rt', a, rec[n][0]), ('sh', b, rec[n][1])):
                if was == 'PASS' and now != 'PASS':
                    print('WORSE %s %s: %s -> %s' % (n, lib, was, now)); worse += 1
                elif was != 'PASS' and now == 'PASS':
                    print('better %s %s: %s -> %s' % (n, lib, was, now)); better += 1
        print('libc-conformance: %d worse, %d better than verdicts.txt' % (worse, better))
        sys.exit(1 if worse else 0)

main()
