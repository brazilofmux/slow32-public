#!/usr/bin/env python3
"""ndxfuzz.py -- xbase's index files held to dBASE III PLUS's, byte for byte.

Each seed is a random dBASE III program: a table of 0 to 700 records
(written here), four indexes built on it -- character (2 to 30 bytes:
12 to 42 keys to a page), numeric, date and a compound key -- then up to
600 appends and key changes with them open, few distinct keys or many,
some dates blank.  dBASE III PLUS runs it under dos-monster, xbase runs
it, and each index dBASE leaves, made what xbase writes where dBASE
leaves bytes as it finds them (normalize.py), must be xbase's byte for
byte.  Not a gate: it needs what the tree does not hold.

  DBASE3=DIR  a dBASE III PLUS 1.1 directory: DBASE.EXE, DBASE.OVL,
              DBASEINL.OVL, DBASE.MSG.  From System Disks 1 and 2, with
              ID.EXE run once there (it asks a name, a company and the
              serial on the System Disk #1 label, and makes DBASE.EXE
              from DBASE.LOD and DBASE.SER).  1.0 is copy-protected.
  DOSMON=PATH dos-monster (default ~/x86/dos-monster)
  XBASE=PATH  the interpreter to hold to it (default xbase/xbase.s32x)

  ndxfuzz.py run [-n N] [-s FIRST] [-o DIR]   seeds FIRST.. (default 1, 50)
  ndxfuzz.py one SEED [-o DIR]                one seed, its files kept

Exit 0 all the same, 1 any differ, 2 something it needs is missing.
"""
import argparse, datetime, os, random, shutil, struct, subprocess, sys, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, '..', '..', '..'))
sys.path.insert(0, HERE)


def workload(seed, d):
    """The table and program for one seed, written into d."""
    r = random.Random(seed)
    w = r.choice([2, 4, 6, 10, 13, 17, 30])
    m = r.choice([0, 5, 40, 300, 700])
    nops = r.choice([50, 200, 600])
    space = r.choice([3, 20, 200, 5000])      # distinct keys: equal keys when few

    def ck(): return ('%0*d' % (w, r.randrange(space)))[-w:] if r.random() < .9 else ''
    def nk(): return r.randrange(-space, space) / 4
    def dk(): return None if r.random() < .05 else r.randrange(space)
    def dlit(v): return 'CTOD("  /  /  ")' if v is None else 'CTOD("01/01/86") + %d' % v

    base = datetime.date(1986, 1, 1)
    recs = []
    for _ in range(m):
        dd = dk()
        ds = (base + datetime.timedelta(days=dd)).strftime('%Y%m%d') if dd is not None else ' ' * 8
        recs.append(b' ' + ck().ljust(w).encode() + ('%9.2f' % nk()).encode() + ds.encode())
    fields = [('C', 'C', w), ('N', 'N', 9), ('D', 'D', 8)]
    h = struct.pack('<BBBBIHH20x', 3, 86, 1, 1, m, 32 + 32 * len(fields) + 1, 1 + w + 9 + 8)
    for nm, t, ln in fields:
        h += nm.encode().ljust(11, b'\0') + t.encode() + b'\0' * 4 + bytes([ln, 2 if t == 'N' else 0]) + b'\0' * 14
    with open(os.path.join(d, 'NKP.DBF'), 'wb') as f:
        f.write(h + b'\r' + b''.join(recs) + b'\x1a')

    p = ['SET TALK OFF', 'SET SAFETY OFF', 'USE NKP', 'INDEX ON C TO RC', 'INDEX ON N TO RN',
         'INDEX ON D TO RD', 'INDEX ON C + DTOC(D) TO RX', 'USE NKP INDEX RC, RN, RD, RX']
    count = m
    for _ in range(nops):
        if r.random() < .4 or count == 0:
            p.append('APPEND BLANK')
            count += 1
            if r.random() < .9:
                p.append('REPLACE C WITH "%s", N WITH %s, D WITH %s' % (ck(), nk(), dlit(dk())))
        else:
            p.append('GO %d' % r.randrange(1, count + 1))
            which = r.random()
            if which < .4: p.append('REPLACE C WITH "%s"' % ck())
            elif which < .6: p.append('REPLACE N WITH %s' % nk())
            elif which < .8: p.append('REPLACE D WITH %s' % dlit(dk()))
            else: p.append('REPLACE C WITH "%s", N WITH %s, D WITH %s' % (ck(), nk(), dlit(dk())))
    p += ['USE', 'QUIT']
    with open(os.path.join(d, 'P.PRG'), 'w') as f:
        f.write('\n'.join(p) + '\n')
    return 'key %d, %d records, %d changes, %d distinct' % (w, m, nops, space)


def run_seed(seed, d, dbase3, dosmon, xbase, emu):
    info = workload(seed, d)
    dd, od = os.path.join(d, 'd'), os.path.join(d, 'o')
    os.makedirs(dd); os.makedirs(od)
    for name in ('DBASE.EXE', 'DBASE.OVL', 'DBASEINL.OVL', 'DBASE.MSG'):
        shutil.copy(os.path.join(dbase3, name), dd)
    with open(os.path.join(dd, 'CONFIG.DB'), 'w', newline='') as f:
        f.write('STATUS=OFF\r\n')
    for sub in (dd, od):
        shutil.copy(os.path.join(d, 'NKP.DBF'), sub)
    with open(os.path.join(d, 'P.PRG')) as f:
        prg = f.read()
    with open(os.path.join(dd, 'P.PRG'), 'w', newline='') as f:
        f.write(prg.replace('\n', '\r\n'))
    shutil.copy(os.path.join(d, 'P.PRG'), od)
    scr = os.path.join(d, 'd.scr')
    # the one key is the Enter that assents to the license screen
    subprocess.run([dosmon, '-W', '-C', dd, '-T', '300', '-D', scr, os.path.join(dd, 'DBASE.EXE'), 'P'],
                   input=b'\r', stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, cwd=dd)
    subprocess.run([emu, xbase], input=b'DO P\nQUIT\n', stdout=subprocess.DEVNULL,
                   stderr=subprocess.DEVNULL, cwd=od)
    with open(scr, 'rb') as f:
        if b'END RUN' not in f.read():
            return info, ['dBASE did not finish (%s)' % scr]
    import normalize
    bad = []
    for k in ('RC', 'RN', 'RD', 'RX'):
        mine = os.path.join(od, k + '.NDX')
        theirs = [n for n in os.listdir(dd) if n.upper() == k + '.NDX']
        if not theirs or not os.path.exists(mine):
            bad.append('%s missing' % k); continue
        want = normalize.normalized(open(os.path.join(dd, theirs[0]), 'rb').read())
        got = open(mine, 'rb').read()
        if want != got:
            at = next((i for i in range(min(len(want), len(got))) if want[i] != got[i]), min(len(want), len(got)))
            bad.append('%s differs at page %d byte %d (%d vs %d bytes)' % (k, at // 512, at % 512, len(want), len(got)))
    return info, bad


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest='cmd', required=True)
    a = sub.add_parser('run'); a.add_argument('-n', type=int, default=50); a.add_argument('-s', type=int, default=1); a.add_argument('-o')
    b = sub.add_parser('one'); b.add_argument('seed', type=int); b.add_argument('-o')
    args = ap.parse_args()
    dbase3 = os.environ.get('DBASE3', '')
    dosmon = os.environ.get('DOSMON', os.path.expanduser('~/x86/dos-monster'))
    xbase = os.environ.get('XBASE', os.path.join(ROOT, 'xbase', 'xbase.s32x'))
    emu = os.path.join(ROOT, 'tools', 'dbt', 'slow32-dbt')
    missing = [w for w, ok in (('DBASE3 (a dBASE III PLUS 1.1 directory)', dbase3 and os.path.exists(os.path.join(dbase3, 'DBASE.EXE'))),
                               ('dos-monster (DOSMON)', os.access(dosmon, os.X_OK)),
                               ('xbase.s32x (XBASE)', os.path.exists(xbase)),
                               ('tools/dbt/slow32-dbt', os.access(emu, os.X_OK))) if not ok]
    if missing:
        print('ndxfuzz: missing ' + ', '.join(missing), file=sys.stderr)
        return 2
    out = args.o or tempfile.mkdtemp(prefix='ndxfuzz.')
    seeds = [args.seed] if args.cmd == 'one' else range(args.s, args.s + args.n)
    fails = 0
    for s in seeds:
        d = os.path.join(out, str(s))
        os.makedirs(d)
        info, bad = run_seed(s, d, dbase3, dosmon, xbase, emu)
        if bad:
            fails += 1
            print('seed %d (%s): %s' % (s, info, '; '.join(bad)))
        else:
            print('seed %d ok (%s)' % (s, info))
            if args.cmd == 'run':
                shutil.rmtree(d)
    print('ndxfuzz: %d of %d differ%s' % (fails, len(seeds), ('; kept in ' + out) if fails or args.cmd == 'one' else ''))
    if not fails and args.cmd == 'run' and not args.o:
        shutil.rmtree(out)
    return 1 if fails else 0


if __name__ == '__main__':
    sys.exit(main())
