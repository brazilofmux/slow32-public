#!/usr/bin/env python3
"""fmut.py run OUT [-n N] [--seed S] [--match RE] [-j J]: accept-invalid differential.

Each of fortran/tests/f77's programs -- valid, every one -- is changed in one
place, N ways, and both compilers are asked only whether they take the result:
gfortran -fsyntax-only, and f77.  A mutant gfortran refuses and f77 compiles is
a statement f77 took in silence: reported, clustered by gfortran's message, the
mutants kept in OUT/found.  (The other way round -- f77 refuses what gfortran
takes -- is no finding: f77 refuses much by name.)  Deterministic for a seed.
OUT must not exist.

A report, not a gate: gfortran refuses things f77 takes on purpose (a second
PROGRAM statement in a main program, as forty FCVS files have it), and
argument mismatches between the units of one file, which f77 does not check.
docs/plans/fortran90.md has what its first run found (2026-11-10: 956 of 9,030
mutants, with every other gate green) and what is left.  ~40 s for -n 30.
"""
import argparse, collections, os, random, re, subprocess, sys, tempfile
from concurrent.futures import ThreadPoolExecutor

ROOT = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..'))
F77 = os.path.join(ROOT, 'fortran/out/f77')
SRC = os.path.join(ROOT, 'fortran/tests/f77')
TOK = re.compile(r"'(?:[^']|'')*'|\"[^\"]*\"|\.[A-Za-z]+\.|[A-Za-z_][A-Za-z0-9_]*|\d+\.?\d*(?:[EeDd][+-]?\d+)?|\*\*|==|/=|<=|>=|=>|::|//|[-+*/=(),:<>%]")
OPS = ['+', '-', '*', '/', '**', '//', '==', '/=', '<', '>', '.AND.', '.OR.', '.NOT.', '.EQ.', '.LT.', '.EQV.', '=', ',', ':', '(', ')']
LITS = ['1', '2.5', "'x'", '.TRUE.', '(1.0,2.0)', '1.0D0']
KEYS = ['INTEGER', 'REAL', 'LOGICAL', 'CHARACTER', 'COMPLEX', 'CALL', 'IF', 'THEN', 'ELSE', 'DO', 'END', 'RETURN',
        'GOTO', 'CONTINUE', 'DIMENSION', 'PARAMETER', 'SAVE', 'DATA', 'COMMON', 'PRINT', 'WRITE', 'READ', 'FUNCTION',
        'SUBROUTINE', 'INTENT', 'POINTER', 'TARGET', 'ALLOCATABLE', 'OPTIONAL', 'RESULT', 'WHERE', 'FORALL', 'TYPE']


def code_lines(lines, fixed):
    """Indices of lines that are statements (not comments, not blank)."""
    out = []
    for i, l in enumerate(lines):
        if not l.strip():
            continue
        if fixed and l[:1] in 'cC*!':
            continue
        if l.lstrip().startswith('!'):
            continue
        out.append(i)
    return out


def mutate(text, fixed, rng):
    lines = text.split('\n')
    idx = code_lines(lines, fixed)
    if not idx:
        return None, ''
    names = sorted(set(m.group(0) for m in re.finditer(r'[A-Za-z_][A-Za-z0-9_]*', text)))
    for _ in range(20):
        li = rng.choice(idx)
        line = lines[li]
        head, body = (line[:6], line[6:]) if fixed else ('', line)
        if '!' in body and "'" not in body and '"' not in body:
            body = body[:body.index('!')]
        toks = [(m.start(), m.end(), m.group(0)) for m in TOK.finditer(body)]
        if not toks:
            continue
        kind = rng.choice(['del', 'op', 'lit', 'name', 'key', 'swap', 'dupline', 'delline'])
        if kind == 'dupline':
            new = lines[:li + 1] + [line] + lines[li + 1:]
            return '\n'.join(new), f'dupline {li + 1}: {line.strip()[:50]}'
        if kind == 'delline':
            new = lines[:li] + lines[li + 1:]
            return '\n'.join(new), f'delline {li + 1}: {line.strip()[:50]}'
        k = rng.randrange(len(toks))
        a, b, t = toks[k]
        if kind == 'del':
            nb = body[:a] + body[b:]
            what = f'del {t}'
        elif kind == 'swap':
            if k + 1 >= len(toks):
                continue
            a2, b2, t2 = toks[k + 1]
            nb = body[:a] + t2 + body[b:a2] + t + body[b2:]
            what = f'swap {t} {t2}'
        else:
            pool = OPS if kind == 'op' else LITS if kind == 'lit' else KEYS if kind == 'key' else names
            r = rng.choice(pool)
            if r.upper() == t.upper():
                continue
            nb = body[:a] + r + body[b:]
            what = f'{t} -> {r}'
        if fixed and len(head + nb) > 72:
            continue
        lines2 = lines[:li] + [head + nb] + lines[li + 1:]
        return '\n'.join(lines2), f'{what}  @{li + 1}: {line.strip()[:50]}'
    return None, ''


def verdict(cmd, cwd):
    try:
        p = subprocess.run(cmd, cwd=cwd, stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=60)
        return p.returncode, (p.stderr + p.stdout).decode('utf8', 'replace')
    except subprocess.TimeoutExpired:
        return -99, 'timeout'


def gf_msg(err):
    m = re.search(r'Error: (.*)', err)
    s = m.group(1) if m else err.strip().split('\n')[-1]
    s = re.sub(r"'[^']*'", 'X', s)
    s = re.sub(r'\(\d+\)', '(N)', s)
    return s[:110]


def one(job):
    name, k, seed, out = job
    path = os.path.join(SRC, name)
    fixed = name.endswith('.f')
    with open(path, encoding='latin-1') as f:
        text = f.read()
    rng = random.Random(f'{seed}/{name}/{k}')
    mut, what = mutate(text, fixed, rng)
    if mut is None:
        return None
    with tempfile.TemporaryDirectory(dir=out) as d:
        src = os.path.join(d, 'm' + os.path.splitext(name)[1])
        with open(src, 'w', encoding='latin-1') as f:
            f.write(mut)
        grc, gerr = verdict(['gfortran', '-fsyntax-only', '-w', src], d)
        if grc == 0 or 'Error' not in gerr:
            return None
        frc, ferr = verdict([F77, src, os.path.join(d, 'm.s')], d)
        if frc == 0:
            keep = os.path.join(out, 'found', f'{os.path.splitext(name)[0]}-{k}{os.path.splitext(name)[1]}')
            with open(keep, 'w', encoding='latin-1') as f:
                f.write(mut)
            return name, k, what, gf_msg(gerr)
        if frc not in (0, 1):
            return name, k, what, f'F77 DIED rc={frc}: {ferr.strip()[:80]}'
    return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('cmd')
    ap.add_argument('out')
    ap.add_argument('-n', type=int, default=20)
    ap.add_argument('--seed', default='1')
    ap.add_argument('--match')
    ap.add_argument('-j', type=int, default=8)
    a = ap.parse_args()
    out = os.path.abspath(a.out)
    os.makedirs(os.path.join(out, 'found'))
    names = sorted(n for n in os.listdir(SRC) if n.endswith(('.f', '.f90')))
    if a.match:
        names = [n for n in names if re.search(a.match, n)]
    jobs = [(n, k, a.seed, out) for n in names for k in range(a.n)]
    found = []
    with ThreadPoolExecutor(a.j) as pool:
        for r in pool.map(one, jobs):
            if r:
                found.append(r)
    c = collections.Counter(r[3] for r in found)
    with open(os.path.join(out, 'found.tsv'), 'w') as f:
        for r in found:
            f.write('\t'.join(str(x) for x in r) + '\n')
    print(f'{len(jobs)} mutants, {len(found)} taken by f77 that gfortran refuses')
    for m, n in c.most_common(60):
        print(f'  {n:5} {m}')


if __name__ == '__main__':
    main()
