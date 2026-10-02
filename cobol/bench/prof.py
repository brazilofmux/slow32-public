#!/usr/bin/env python3
"""prof.py prog.s32x prog.prof NAMES [generated-symbol ...] [-n N]

Symbolize a `slow32 -p` profile (instructions executed per code address)
and say where a COBOL program's instructions go: its own generated code,
libcob, the C library -- and, apart, what the DBT runs natively (the
hooked numeric and edit kernels, mem*), which costs the guest nothing
there.  "Guest-only" is the rest: what the DBT actually translates.

Symbols come from the .s32x's .symtab.  NAMES lists libcob's functions
(prof.sh writes it, with a libcob whose every function, static ones too,
has a global __p_ alias).  The generated symbols are the program-ids and
function-ids, lower case, '-' as '_'; prof.sh finds them.  Used by
bench/prof.sh; docs/performance.md has the method."""
import struct, sys, bisect, os, collections
x, pf, namesf = sys.argv[1], sys.argv[2], sys.argv[3]
N = int(sys.argv[sys.argv.index('-n') + 1]) if '-n' in sys.argv else 25
d = open(x, 'rb').read()
nsec, secoff, stroff, strsz = struct.unpack_from('<IIII', d, 0x0C)
def cstr(base, o):
    e = d.index(b'\0', base + o); return d[base + o:e].decode('latin1')
secs = {}
for i in range(nsec):
    no, ty, va, off, sz, msz, fl = struct.unpack_from('<7I', d, secoff + 28 * i)
    secs[cstr(stroff, no)] = (va, off, sz, msz)
so, st = secs['.symtab'][1], secs['.sym_strtab'][1]
nsym = (st - so) // 16
text_end = secs['.text'][3] or secs['.text'][2]
syms = []
for i in range(nsym):
    no, val, sec, ty, bind, size = struct.unpack_from('<IIHBBI', d, so + 16 * i)
    name = cstr(st, no)
    if val < text_end and name and not name.startswith('.L'): syms.append((val, name))
syms.sort()
# several names at one address: prefer the real name over the alias
addr = []; names = []
for v, n in syms:
    if addr and addr[-1] == v:
        if names[-1].startswith('__p_') : pass
        elif n.startswith('__p_'): names[-1] = n
        continue
    addr.append(v); names.append(n)
libcob = set(open(namesf).read().split())
tot = collections.Counter(); total = 0
for line in open(pf):
    a, c = line.split(); a = int(a, 16); c = int(c)
    k = bisect.bisect_right(addr, a) - 1
    n = names[k] if k >= 0 else '?'
    if n.startswith('__p_'): n = n[4:]
    tot[n] += c; total += c
HOOKED = {'cob_k_get_num','cob_k_put_num','cob_k_put_scale','cob_k_get_edited','cob_k_put_edited','cob_k_get_ok','cob_k_put_ok','cob_k_ed_ok',
          'cob_get_num','cob_get_num_impl','cob_put_num_x','cob_put_num_x_impl','cob_get_edited','cob_get_edited_impl','cob_put_edited','cob_put_edited_impl',
          'cob_edit_apply','cob_deedit','strchr',
          'memcpy','memset','memmove','memcmp','strlen','strcpy','strcmp','memswap','llvm.memcpy.p0.p0.i32','llvm.memset.p0.i32','llvm.memmove.p0.p0.i32'}
def cat(n):
    if n in HOOKED or n.startswith('__s32hk_'): return 'native'
    if n in libcob: return 'libcob'
    if n in gen: return 'generated'
    return 'libc/rt'
gen = set(a for a in sys.argv[4:] if not a.startswith('-') and not a.isdigit())
cats = collections.Counter()
for n, c in tot.items(): cats[cat(n)] += c
guest = total - cats['native']
print('total %d instructions; native under the DBT (hooks, mem*) %.1f%%; guest-only %d' % (total, 100.0 * cats['native'] / total, guest))
for k in ('generated', 'libcob', 'libc/rt'): print('  %-10s %5.1f%% of guest-only  %d' % (k, 100.0 * cats[k] / guest, cats[k]))
k = 0
for n, c in tot.most_common():
    if cat(n) == 'native': continue
    print('  %5.1f%%  %-10s %s' % (100.0 * c / guest, cat(n), n)); k += 1
    if k >= N: break
