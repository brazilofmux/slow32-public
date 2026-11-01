#!/usr/bin/env python3
"""Flag text copied from the standard: any run of 12 words a file shares
with N1570.  usage: ngram.py n1570.txt file...   (exit 1 on a hit)
Probes and pages paraphrase and cite clauses; they never quote."""
import re, sys
N = 12
def words(t):
    return re.findall(r"[a-z0-9_]+", t.lower())
std = words(open(sys.argv[1], errors='replace').read())
grams = set(tuple(std[i:i + N]) for i in range(len(std) - N + 1))
hits = 0
for f in sys.argv[2:]:
    w = words(open(f, errors='replace').read())
    for i in range(len(w) - N + 1):
        if tuple(w[i:i + N]) in grams:
            print('%s: %s' % (f, ' '.join(w[i:i + N]))); hits += 1
            break
sys.exit(1 if hits else 0)
