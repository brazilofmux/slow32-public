#!/usr/bin/env python3
"""sites.py prog.s32x prog.prof [-t target ...] [-n N]: call sites by count --
every `jal lr, X` in the text with how often it ran (the profile's count at
that address), the function it sits in and the nearest -fprofile-lines label
before it (the source line)."""
import struct, sys, bisect, subprocess, collections, os, re
x, pf = sys.argv[1], sys.argv[2]
targets = set(); N = 30
a = sys.argv[3:]
while a:
    if a[0] == '-t': targets.add(a[1]); a = a[2:]
    elif a[0] == '-n': N = int(a[1]); a = a[2:]
    else: a = a[1:]
d = open(x, 'rb').read()
nsec, secoff, stroff, strsz = struct.unpack_from('<IIII', d, 0x0C)
def cstr(base, o):
    e = d.index(b'\0', base + o); return d[base + o:e].decode('latin1')
secs = {}
for i in range(nsec):
    no, ty, va, off, sz, msz, fl = struct.unpack_from('<7I', d, secoff + 28 * i)
    secs[cstr(stroff, no)] = (va, off, sz, msz)
so, st = secs['.symtab'][1], secs['.sym_strtab'][1]
syms = []
for i in range((st - so) // 16):
    no, val, sec, ty, bind, size = struct.unpack_from('<IIHBBI', d, so + 16 * i)
    name = cstr(st, no)
    if name and not name.startswith('.L'): syms.append((val, name))
syms.sort()
fa, fn, la, ln = [], [], [], []
for v, n in syms:
    if n.startswith('__ln_'): la.append(v); ln.append(n)
    elif not n.startswith('__p_'): fa.append(v); fn.append(n)
def owner(ad):
    k = bisect.bisect_right(fa, ad) - 1; return fn[k] if k >= 0 else '?'
def line(ad):
    k = bisect.bisect_right(la, ad) - 1
    if k < 0: return '-'
    # a label belongs to the owner's own code only
    return ln[k].split('_')[3] if owner(la[k]) == owner(ad) else '-'
cnt = {}
for l in open(pf):
    s, c = l.split(); cnt[int(s, 16)] = int(c)
dis = subprocess.run([os.path.expanduser('~/slow-32/tools/utilities/slow32dis'), x], capture_output=True, text=True).stdout
rows = []
for m in re.finditer(r'^\s+0x([0-9a-f]+):\s+[0-9a-f]+\s+jal\s+lr,\s*(\S+)', dis, re.M):
    ad = int(m.group(1), 16); t = m.group(2)
    if targets and t not in targets: continue
    c = cnt.get(ad, 0)
    if c: rows.append((c, t, ad, owner(ad), line(ad)))
rows.sort(reverse=True)
per = collections.Counter()
for c, t, ad, o, l in rows: per[t] += c
print("%10s  %-22s %-10s %-16s %s" % ("calls", "target", "site", "in", "line"))
for c, t, ad, o, l in rows[:N]: print("%10d  %-22s 0x%08x %-16s %s" % (c, t, ad, o, l))
if not targets:
    print("-- per target:")
    for t, c in per.most_common(25): print("%10d  %s" % (c, t))
