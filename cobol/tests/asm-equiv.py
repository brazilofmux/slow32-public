#!/usr/bin/env python3
# asm-equiv.py A.s B.s -- exit 0 when two assemblies are the same code up
# to the compiler's label numbering and the order of data definitions:
# every .L<family><n>[_<n>] label is renamed in order of first appearance,
# the code (each stretch before a data section switch) must match line for
# line, and all lines must match as a multiset.  For a refactor that moves
# when labels, literals or descriptors are allocated but not what the code
# does (docs/plans/frontend-pass.md, step 4: nested statements parsed
# before the code around them).  With DIR1 DIR2 it checks every .s file
# that differs between two tests/asm-snapshot.sh trees, listing the ones
# that are not the same code.
import os, re, sys
from collections import Counter

LAB = re.compile(r'\.L[a-z]*\d+(?:_\d+)*\b')

def norm(path):
    m = {}
    def sub(mo):
        k = mo.group(0)
        if k not in m: m[k] = '.N%d' % len(m)
        return m[k]
    with open(path, errors='replace') as f:
        return [LAB.sub(sub, l) for l in f.read().split('\n')]

def code(lines):
    out, text = [], True
    for l in lines:
        s = l.strip()
        if s in ('.data', '.bss') or s.startswith('.section'): text = False
        elif s == '.text': text = True
        if text: out.append(l)
    return out

def same(a, b):
    x, y = norm(a), norm(b)
    return code(x) == code(y) and Counter(x) == Counter(y)

def main():
    a, b = sys.argv[1], sys.argv[2]
    if os.path.isfile(a): sys.exit(0 if same(a, b) else 1)
    bad = n = 0
    for root, _, files in os.walk(a):
        for fn in files:
            if not fn.endswith('.s'): continue
            p = os.path.join(root, fn); q = os.path.join(b, os.path.relpath(p, a))
            if not os.path.exists(q): continue
            if open(p, 'rb').read() == open(q, 'rb').read(): continue
            n += 1
            if not same(p, q): bad += 1; print('differs:', os.path.relpath(p, a))
    print('%d changed, %d not the same code' % (n, bad))
    sys.exit(1 if bad else 0)

main()
