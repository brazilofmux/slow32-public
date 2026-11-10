#!/usr/bin/env python3
"""gfortran-suite.py OUT [--f77] [-j N] [--match RE] [--sample K]: gfortran's own
execute tests through flang on SLOW-32, native gfortran as the oracle.  With --f77
the compiler is fortran/'s f77 instead: most of the corpus is past Fortran 95 and
f77 refuses it by name (`f77`), so what to read there is everything else -- a
program it took and ran to another answer, one it died on (`f77-crash`), one whose
assembly or link failed.

The tests come from LLVM's test suite (Fortran/gfortran/regression; GPL, kept in
~/refs and never in git; SUITE= says where).  Each is built and run both ways.  A
test only counts against flang if gfortran compiles it and it passes there, so a
test that needs gfortran's extensions or Mac behaviour is `oracle-*`, not a
failure.  OUT must not exist; it gets results.tsv, summary.txt and a directory per
test that did not pass.

  pass          same exit (zero or not) and same stdout
  format-only   same exit; same words and numbers, spaced or rounded otherwise
                (list-directed output is the processor's)
  text-differs  same exit; stdout really differs
  exit-differs  one exited zero and the other did not
  timeout       under the engine
  flang llc as ld   the phase that refused it (f77, f77-crash under --f77)
"""
import argparse, collections, fnmatch, os, re, subprocess, sys, tempfile
from concurrent.futures import ThreadPoolExecutor

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
SUITE = os.environ.get('SUITE', os.path.expanduser('~/refs/llvm-test-suite/Fortran/gfortran/regression'))
DBT = os.path.join(ROOT, 'tools/dbt/slow32-dbt')
DRIVER = os.path.join(ROOT, 'flang/s32flang')
FC = os.environ.get('GFORTRAN', 'gfortran')
EXT = re.compile(r'\.(f|f90|f95|f03|f08|for)$', re.I)
# Not what this measures: another machine's parallelism, sanitizers, debuggers.
SKIPDIRS = {'coarray', 'gomp', 'goacc', 'asan', 'ubsan', 'lto', 'guality', 'debug', 'vect', 'graphite', 'analyzer'}
PASS_ON = re.compile(r'^-(fdefault-(real|integer|double)-\d+|ffree-form|ffixed-form|fbackslash|ffixed-line-length-\S+|funsigned|cpp)$')
PHASE = {11: 'flang', 12: 'llc', 13: 'as', 14: 'ld', 15: 'clang'}
HOST_TIMEOUT, BUILD_TIMEOUT, RUN_TIMEOUT = 30, 300, 120
# The tests' C is written to gfortran's ISO_Fortran_binding.h.  flang's spells these types
# otherwise, in a helper that dumps descriptors to stderr, and doesn't bring in stdint.h.
F77 = os.path.join(ROOT, 'fortran/out/f77')
F77_LIB = os.path.join(ROOT, 'fortran/runtime/libf77.s32o')
AS = os.path.join(ROOT, 'tools/assembler/slow32asm')
LD = os.path.join(ROOT, 'tools/linker/s32-ld')
RT = os.path.join(ROOT, 'runtime')
# Options f77 has no way to honour, and that change what a program means.
F77_CANNOT = re.compile(r'^-(fdefault-|ffree-form|ffixed-form|fbackslash|ffixed-line-length|funsigned|cpp|fdec|fcray-pointer|fno-range-check|frecursive|fmax-stack|fdollar-ok|fno-underscoring|fsecond-underscore|fd-lines|fall-intrinsics|fwhole-file|fno-whole-file|fpack-derived|fshort-enums|fno-align|fexternal-blas|fblas|ffrontend-|fstack-arrays|fno-realloc|fprotect-parens|fno-protect|ffast-math|fno-signed-zeros|funsafe|ffinite|fno-math-errno|fno-trapping|fassociative|freciprocal|fno-backtrace|fconvert|frecord-marker|fno-f2c|ff2c|fno-second)')
COMPILER = 'flang'
C_FOR_FLANG = ['-DCFI_type_ucs4_char=CFI_type_char32_t', '-DCFI_type_cfunptr=CFI_type_cptr', '-include', 'stdint.h']


def directives(text):
    run = re.search(r'dg-do\s+run(\s*\{[^}]*\})?', text)
    opts = []
    for m in re.finditer(r'dg-(?:additional-)?options\s+"([^"]*)"(\s*\{[^}]*target[^}]*\})?', text):
        if not m.group(2):
            opts += m.group(1).split()
    return run, opts, 'dg-shouldfail' in text


# What DejaGnu would ask of a target, answered for this one.  INTEGER(16), REAL(10) and
# (16) do not exist here.  Whether the C library can truncate a file and so on is for the
# tests to show.  An xfail isn't a condition: the oracle says what it does on this host.
TRIPLE = 'slow32-unknown-none'
EFFECTIVE = dict.fromkeys(('fd_truncate', 'c99_runtime', 'stdint_types', 'le', 'ilp32', 'nonpic',
                           'fortran_largest_fp_has_sqrt'), True)
EFFECTIVE.update(dict.fromkeys(('fortran_integer_16', 'fortran_large_int', 'fortran_large_real', 'fortran_real_10', 'fortran_real_16', 'fortran_real_c_float128',
                                'lto', 'static_libgfortran', 'be', 'ia32', 'newlib', 'pie_enabled', 'sse2_runtime'), False))


def selector(tokens):
    """A DejaGnu selector: names and triplets, {}, !, && and ||; beside each other, any."""
    def unary(i):
        t = tokens[i]
        if t == '!':
            v, i = unary(i + 1)
            return not v, i
        if t == '{':
            v, i = expr(i + 1)
            return v, i + 1
        if '-' in t:
            return fnmatch.fnmatchcase(TRIPLE, t), i + 1
        if t not in EFFECTIVE:
            sys.exit(f'gfortran-suite: no answer for the target selector {t}')
        return EFFECTIVE[t], i + 1

    def expr(i):
        v, op = None, 'or'
        while i < len(tokens) and tokens[i] != '}':
            if tokens[i] in ('||', '&&'):
                op = 'or' if tokens[i] == '||' else 'and'
                i += 1
                continue
            u, i = unary(i)
            v = u if v is None else (v or u) if op == 'or' else (v and u)
            op = 'or'
        return bool(v), i

    return expr(0)[0]


def applies(text):
    """Would this test run on SLOW-32?"""
    words = lambda s: re.findall(r'[{}]|[^\s{}]+', s)
    for line in text.splitlines():
        m = re.search(r'dg-do\s+run\s*\{\s*target\s+(.*)', line)
        if m and not selector(words(m.group(1))):
            return False
        m = re.search(r'dg-require-effective-target\s+(\S+)\s*(?:\{\s*target\s+(.*))?', line)
        if m:
            name = m.group(1).rstrip('}')
            if name and (not m.group(2) or selector(words(m.group(2)))) and not selector([name]):
                return False
    return True


def companions(src, text):
    """What else the test is made of: Fortran read for its modules, and sources linked in."""
    def named(what):
        return [os.path.join(os.path.dirname(src), x) for m in re.finditer(r'dg-' + what + r'\s+"?\{?([^"}\n]*)', text)
                for x in m.group(1).split()]
    return named('compile-aux-modules'), named('additional-sources')


def driver(src, text, work, opts, target='t.s32x', extra_env=None):
    """The command and environment that build a test for SLOW-32."""
    aux, more = companions(src, text)
    env = dict(os.environ, FFLAGS=' '.join(o for o in opts if PASS_ON.match(o)), AUX_MODULES=' '.join(aux),
               CFLAGS=' '.join(C_FOR_FLANG))
    env.update(extra_env or {})
    return ['zsh', DRIVER, src, os.path.join(work, target)] + more, env


def build_f77(src, text, work, opts):
    """The phase that stopped the build (None if none) and what it said."""
    aux, more = companions(src, text)
    if any(not EXT.search(m) for m in more):
        return 'f77', 'a source that is not Fortran'
    objs = []
    for i, f in enumerate(aux + [src] + more):
        o = os.path.join(work, f't{i}')
        rc, out, err = proc([F77, f, o + '.s'], work, BUILD_TIMEOUT)
        if rc is None:
            return 'build-timeout', ''
        if rc != 0:
            said = (err + out).decode('utf8', 'replace').strip().splitlines()
            # f77 says "f77: line N: ..." and exits 1; anything else is the compiler dying
            refused = rc == 1 and said and said[0].startswith('f77:')
            return ('f77' if refused else 'f77-crash'), (said[0] if said else f'rc={rc}')
        rc, out, err = proc([AS, o + '.s', o + '.s32o'], work, BUILD_TIMEOUT)
        if rc != 0:
            return 'as', first_error(out + err)
        objs.append(o + '.s32o')
    rc, out, err = proc([LD, '-o', os.path.join(work, 't.s32x'), '--mmio', '64K', os.path.join(RT, 'crt0.s32o')] + objs
                        + [F77_LIB, os.path.join(RT, 'libc_mmio.s32a'), os.path.join(RT, 'libs32.s32a')], work, BUILD_TIMEOUT)
    if rc != 0:
        return 'ld', first_error(out + err)
    return None, ''


def build_flang(src, text, work, opts):
    cmd, env = driver(src, text, work, opts)
    rc, _, err = proc(cmd, work, BUILD_TIMEOUT, env)
    if rc is None:
        return 'build-timeout', ''
    if rc != 0:
        return PHASE.get(rc, f'build-rc{rc}'), first_error(err)
    return None, ''


def skip_reason(rel, text, opts):
    parts = rel.split('/')
    if len(parts) > 1 and parts[0] in SKIPDIRS:
        return 'skip-dir'
    if not applies(text):
        return 'skip-conditional'
    if any(o.startswith(('-fopenmp', '-fopenacc', '-fcoarray=lib', '-flto', '-fsanitize')) for o in opts):
        return 'skip-option'
    if any(o.startswith(('-fcheck', '-fbounds-check', '-finit-', '-fsign-zero', '-fno-sign-zero', '-fno-automatic', '-freal-')) for o in opts):
        return 'skip-flag'  # gfortran's run-time checks, fill values, kind remapping; flang has no such options
    if '-ff2c' in opts and 'dg-additional-sources' in text:
        return 'skip-flag'  # C on the other side expecting f2c's names; alone, the flag changes nothing to test
    if COMPILER == 'f77' and any(F77_CANNOT.match(o) for o in opts):
        return 'skip-flag'
    return None


def proc(cmd, cwd, timeout, env=None):
    try:
        p = subprocess.run(cmd, cwd=cwd, env=env, timeout=timeout, stdin=subprocess.DEVNULL,
                           stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        return p.returncode, p.stdout, p.stderr
    except subprocess.TimeoutExpired:
        return None, b'', b''


def first_error(err):
    lines = err.decode('utf8', 'replace').strip().splitlines()
    for line in lines:  # "Semantic errors in x.f90" says nothing; the next line does
        if re.search(r'error: (Semantic errors|Could not parse)', line):
            continue
        if 'error:' in line or 'not yet implemented' in line or 'undefined' in line.lower() or 'fatal' in line.lower():
            return line.strip()
    return lines[0].strip() if lines else ''


def cluster(msg):
    """The error with its file, numbers and names taken out."""
    m = re.search(r'(?:error|fatal error|not yet implemented):?\s*(.*)', msg)
    s = m.group(1) if m else msg
    s = re.sub(r'^\(?/\S*\)?:?\s*', '', s)
    s = re.sub(r"'[^']*'", 'X', s)
    s = re.sub(r'\b\d+\b', 'N', s)
    return s[:90]


def same_tokens(a, b):
    """The same words and, to single precision, numbers: list-directed spacing and
    digits are the processor's."""
    split = lambda t: re.findall(r'[^\s(),;]+|[(),;]', t.decode('latin-1'))
    x, y = split(a), split(b)
    if len(x) != len(y):
        return False
    for p, q in zip(x, y):
        if p == q:
            continue
        try:
            u, v = float(p.replace('D', 'E')), float(q.replace('D', 'E'))
        except ValueError:
            return False
        if abs(u - v) > 1e-5 * max(abs(u), abs(v), 1e-30):
            return False
    return True


def one(rel, out):
    src = os.path.join(SUITE, rel)
    with open(src, encoding='latin-1') as f:
        text = f.read()
    run, opts, shouldfail = directives(text)
    why = skip_reason(rel, text, opts)
    if why:
        return rel, why, ''
    name = rel.replace('/', '__')
    work = os.path.join(out, 'w', name)
    os.makedirs(work)

    # the oracle first: if it has no verdict, flang's costs nothing
    aux, more = companions(src, text)
    for m in aux:
        proc([FC, '-w', '-O1', '-c'] + opts + [m], work, BUILD_TIMEOUT)  # for its .mod
    rc, _, err = proc([FC, '-w', '-O1'] + opts + [src] + more + ['-o', 'h'], work, BUILD_TIMEOUT)
    if rc != 0:
        return rel, 'oracle-refuses', first_error(err)
    hrc, hout, _ = proc(['./h'], work, HOST_TIMEOUT)
    os.unlink(os.path.join(work, 'h'))
    if hrc is None:
        return rel, 'oracle-timeout', ''
    if (hrc != 0) != shouldfail:
        return rel, 'oracle-fails', f'rc={hrc}'

    phase, said = (build_f77 if COMPILER == 'f77' else build_flang)(src, text, work, opts)
    if phase:
        return rel, phase, said
    trc, tout, terr = proc([DBT, 't.s32x'], work, RUN_TIMEOUT)
    if trc is None:
        verdict, note = 'timeout', ''
    elif (trc != 0) != (hrc != 0):
        verdict, note = 'exit-differs', f'gfortran rc={hrc} {COMPILER} rc={trc} {first_error(terr)[:120]}'
    elif tout != hout:
        verdict, note = ('format-only', '') if same_tokens(hout, tout) else ('text-differs', '')
    else:
        verdict, note = 'pass', ''
    for n in ('t.s32x', 't.s32o', 't.s', 't.ll') + tuple(f't{i}.{e}' for i in range(8) for e in ('s', 's32o')):
        p = os.path.join(work, n)
        if os.path.exists(p) and (verdict == 'pass' or n.endswith('.s32o')):
            os.unlink(p)
    if verdict != 'pass':
        with open(os.path.join(work, 'gfortran.out'), 'wb') as f:
            f.write(hout)
        with open(os.path.join(work, COMPILER + '.out'), 'wb') as f:
            f.write(tout)
        with open(os.path.join(work, COMPILER + '.err'), 'wb') as f:
            f.write(terr)
    return rel, verdict, note


HEADER = """# flang/tests/gfortran-suite.py's verdict on each of gfortran's execute tests.
# regression/run-flang-corpus.sh holds a run to this; --update records one.
"""
AGREE = ('pass', 'format-only')
# More than this leaving for "gfortran has no verdict" is the oracle broken, not a
# new gfortran's opinion of a few tests.
ORACLE_SLACK = 20


def check(path, results, out):
    """0 if nothing that agreed stopped agreeing, and every test was run."""
    recorded = {}
    with open(path) as f:
        for line in f:
            if not line.startswith('#'):
                v, t = line.rstrip('\n').split('\t')
                recorded[t] = v
    now = {t: v for t, v, _ in results}
    bad = 0
    for t in sorted(set(recorded) - set(now)):
        print(f'MISSING   {t}: recorded {recorded[t]}, not run')
        bad += 1
    for t in sorted(set(now) - set(recorded)):
        print(f'NEW       {t}: {now[t]}, not recorded (--update)')
        bad += 1
    lost = []
    oracle = 0
    for t in sorted(set(recorded) & set(now)):
        was, is_ = recorded[t], now[t]
        if was in AGREE and is_ not in AGREE:
            if is_.startswith('oracle-'):
                oracle += 1
                print(f'NOTE      {t}: gfortran has no verdict now ({is_})')
            else:
                lost.append(t)
        elif was not in AGREE and is_ in AGREE:
            print(f'BETTER    {t}: {was} -> {is_} (--update to keep it)')
        elif was != is_ and was not in AGREE:
            print(f'NOTE      {t}: {was} -> {is_}')
    if oracle > ORACLE_SLACK:
        print(f'WORSE     {oracle} tests lost their oracle: is gfortran working?')
        bad += 1
    # A timeout under load is not a regression: alone, it has to fail again.
    retry = os.path.join(out, 'retry')
    os.makedirs(retry)
    for t in lost:
        _, v, note = one(t, retry)
        if v in AGREE:
            print(f'NOTE      {t}: {now[t]} under load, {v} alone')
        else:
            print(f'WORSE     {t}: {recorded[t]} -> {v}  {note[:90]}')
            bad += 1
    print(f'{COMPILER} corpus: {sum(v in AGREE for v in now.values())} agree, {bad} worse than recorded')
    return 1 if bad else 0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('out')
    ap.add_argument('-j', type=int, default=8)
    ap.add_argument('--match')
    ap.add_argument('--f77', action='store_true', help="fortran/'s f77 in flang's place")
    ap.add_argument('--sample', type=int, help='every Kth test')
    ap.add_argument('--check', help='hold the run to these recorded verdicts; exit 1 if worse')
    ap.add_argument('--update', help='record the verdicts of this run here')
    a = ap.parse_args()
    global COMPILER
    if a.f77:
        COMPILER = 'f77'
    out = os.path.abspath(a.out)
    os.makedirs(out)  # refuses one that exists
    tests = []
    for d, _, fs in os.walk(SUITE):
        for f in fs:
            if EXT.search(f):
                rel = os.path.relpath(os.path.join(d, f), SUITE)
                with open(os.path.join(d, f), encoding='latin-1') as h:
                    if re.search(r'dg-do\s+run', h.read()):
                        tests.append(rel)
    tests.sort()
    if a.match:
        tests = [t for t in tests if re.search(a.match, t)]
    if a.sample:
        tests = tests[::a.sample]
    results = []
    with ThreadPoolExecutor(a.j) as pool, open(os.path.join(out, 'results.tsv'), 'w') as tsv:
        for r in pool.map(lambda t: one(t, out), tests):
            results.append(r)
            tsv.write('\t'.join(r) + '\n')
            tsv.flush()
    counts = collections.Counter(v for _, v, _ in results)
    lines = [f'{len(results)} tests'] + [f'  {v:18} {n}' for v, n in counts.most_common()]
    judged = sum(n for v, n in counts.items() if not v.startswith(('oracle-', 'skip-')))
    if judged:
        good = counts['pass'] + counts['format-only']
        lines.append(f'agree where gfortran has a verdict: {good}/{judged} ({100 * good / judged:.1f}%)')
    for phase in ('flang', 'f77', 'f77-crash', 'clang', 'llc', 'as', 'ld', 'exit-differs'):
        c = collections.Counter(cluster(n) for _, v, n in results if v == phase)
        if c:
            lines.append(f'--- {phase}')
            lines += [f'  {n:5} {m}' for m, n in c.most_common(40)]
    report = '\n'.join(lines) + '\n'
    with open(os.path.join(out, 'summary.txt'), 'w') as f:
        f.write(report)
    sys.stdout.write(report)
    if a.update:
        with open(a.update, 'w') as f:
            f.write(HEADER + ''.join(f'{v}\t{t}\n' for t, v, _ in sorted(results)))
        print(f'recorded {len(results)} verdicts in {a.update}')
    if a.check:
        sys.exit(check(a.check, results, out))


if __name__ == '__main__':
    main()
