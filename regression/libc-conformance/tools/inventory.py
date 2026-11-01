#!/usr/bin/env python3
"""Clause 7 inventory: is each Annex B identifier provided, per library?

usage: inventory.py annexb.tsv out.tsv [leg ...]   legs: clang stage08 host
Each identifier gets, per leg: MACRO, FUNC/OBJ (address taken, links),
TYPE, DECL-ONLY (compiles, does not link), MACRO-ONLY (a library function
present only as a macro: 7.1.4p1 wants a real function too), MISSING.
"""
import os, re, subprocess, sys, tempfile, concurrent.futures as cf

ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), '../../..'))
LLVM = os.environ.get('LLVM_BIN', os.path.expanduser('~/llvm-project/build/bin'))
CLANG = LLVM + '/clang'
LLC = LLVM + '/llc'
AS = ROOT + '/tools/assembler/slow32asm'
LD = ROOT + '/tools/linker/s32-ld'
DBT = ROOT + '/tools/dbt/slow32-dbt'
CC8 = ROOT + '/selfhost/stage08/cc.s32x'
L8 = ROOT + '/selfhost/stage08/lib'
OPTIONAL_HDRS = {'complex.h', 'stdatomic.h', 'threads.h'}

def annex_k(name):
    return (name.endswith('_s') or name.endswith('_S') or name in
            ('errno_t', 'rsize_t', 'RSIZE_MAX', 'constraint_handler_t',
             '__STDC_WANT_LIB_EXT1__', '__STDC_LIB_EXT1__'))

PATS = [r'^(PRI|SCN)([diouxX])(N|LEASTN|FASTN)$', r'^(u?int)(N|_leastN|_fastN)_t$',
        r'^(U?INT)(N|_LEASTN|_FASTN)_(MIN|MAX)$', r'^(U?INT)N_C$']
def expand(name):
    for p in PATS:
        if re.match(p, name):
            return [re.sub(r'N(?=(_|$))', str(n), name, count=1) if 'LEASTN' not in name and 'FASTN' not in name
                    else name.replace('LEASTN', 'LEAST%d' % n).replace('FASTN', 'FAST%d' % n).replace('leastN', 'least%d' % n).replace('fastN', 'fast%d' % n)
                    for n in (8, 16, 32, 64)]
    return [name]

def run(cmd, timeout=600):
    try:
        r = subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=timeout)
        return r.returncode == 0
    except subprocess.TimeoutExpired:
        return False

def build(leg, src, d, link):
    """compile (and optionally link) src; True on success"""
    if leg == 'host':
        args = ['cc', '-std=c11', '-w', src, '-o', d + '/h'] + ([] if link else ['-c'])
        if not link: args[-2] = d + '/h.o'
        return run(args + (['-lm'] if link else []))
    if leg == 'clang':
        if not run([CLANG, '-target', 'slow32-unknown-none', '-std=c11', '-w', '-S', '-emit-llvm', '-O1',
                    '-I' + ROOT + '/runtime/include', src, '-o', d + '/c.ll']): return False
        if not run([LLC, '-mtriple=slow32-unknown-none', d + '/c.ll', '-o', d + '/c.s']): return False
        if not run([AS, d + '/c.s', d + '/c.s32o']): return False
        if not link: return True
        return run([LD, '-o', d + '/c.s32x', '--mmio', '64K', ROOT + '/runtime/crt0.s32o', d + '/c.s32o',
                    ROOT + '/runtime/libc_mmio.s32a', ROOT + '/runtime/libs32.s32a'])
    if leg == 'stage08':
        if not run([DBT, CC8, '-I' + ROOT + '/selfhost/stage08/include', src, d + '/s.s']): return False
        if not os.path.exists(d + '/s.s') or os.path.getsize(d + '/s.s') == 0: return False
        if not run([AS, d + '/s.s', d + '/s.s32o']): return False
        if not link: return True
        return run([LD, '-o', d + '/s.s32x', '--mmio', '64K', L8 + '/crt0.s32o', d + '/s.s32o', SELF_LIBC])

def probe(leg, hdr, kind, name):
    with tempfile.TemporaryDirectory() as d:
        src = d + '/t.c'
        def tu(body):
            open(src, 'w').write('#include <%s>\n%s\n' % (hdr, body))
        if not name.startswith('struct '):
            tu('#ifndef %s\n#error no\n#endif\nint main(void){return 0;}' % name)
            macro = build(leg, src, d, False)
        else:
            macro = False
        if macro and kind != 'func':
            return 'MACRO'
        if not name.startswith('struct '):
            tu('int main(void){ void *volatile p = (void *)&(%s); return p == 0; }' % name)
            if build(leg, src, d, True):
                return 'FUNC' if kind == 'func' else 'OBJ'
            compiled = build(leg, src, d, False)
            if compiled:
                return 'DECL-ONLY'
            if macro:
                return 'MACRO-ONLY'
        tu('typedef %s t__;\nint main(void){ return sizeof(t__) == 0; }' % name)
        if build(leg, src, d, False):
            return 'TYPE'
        tu('typedef %s t__;\nt__ *volatile q__;\nint main(void){ return q__ == 0; }' % name)
        if build(leg, src, d, False):
            return 'TYPE-INCOMPLETE'
        tu('int main(void){ return (int)sizeof(%s) == 0; }' % name)
        if build(leg, src, d, True):
            return 'OBJ'
        return 'MISSING'

def main():
    global SELF_LIBC
    rows = [l.rstrip('\n').split('\t') for l in open(sys.argv[1]) if l.strip()]
    legs = sys.argv[3:] or ['clang', 'stage08', 'host']
    d = tempfile.mkdtemp()
    SELF_LIBC = d + '/libc.s32a'
    members = [L8 + '/' + f for f in sorted(os.listdir(L8)) if f.endswith('.s32o') and f != 'crt0.s32o']
    subprocess.run([ROOT + '/tools/utilities/s32-ar', 'rc', SELF_LIBC] + members, stdout=subprocess.DEVNULL)
    items = []
    for hdr, kind, name in rows:
        if hdr in OPTIONAL_HDRS or annex_k(name):
            continue
        for n in expand(name):
            items.append((hdr, kind, n))
    jobs = {}
    with cf.ThreadPoolExecutor(max_workers=int(os.environ.get('JOBS', '12'))) as ex:
        for it in items:
            for leg in legs:
                jobs[(it, leg)] = ex.submit(probe, leg, *it)
    with open(sys.argv[2], 'w') as f:
        f.write('header\tkind\tname\t' + '\t'.join(legs) + '\n')
        for it in items:
            f.write('\t'.join(it) + '\t' + '\t'.join(jobs[(it, l)].result() for l in legs) + '\n')

main()
