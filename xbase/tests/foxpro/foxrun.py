#!/usr/bin/env python3
"""foxrun.py -- programs run by FoxPro 2.6 for Windows, the oracle beside dBASE.

A case is a directory holding P.PRG and whatever tables it reads.  Every
case named runs in one boot: Windows 3.11 under dos-monster, FoxPro 2.6
started on a driver program that runs each P.PRG with SET ALTERNATE on
(OUT.TXT), every error it raises logged and passed over (ERR.TXT), and
RAN.TXT written when it returns.  Then FoxPro leaves Windows and DONE.COM
writes the isa-debug-exit port.  Each case directory comes back with
everything FoxPro left in it: OUT.TXT, ERR.TXT, the tables, indexes and
memo files it wrote.  `diff` runs the same cases under xbase too, in the
same frame (ERR.TXT, RAN.TXT; the error log in a program of its own, since
dBASE III finds no procedure further down its caller), and requires the
same OUT.TXT and as many errors.  Not a gate: it needs what the tree does
not hold.  A difference is a question, not a verdict: xbase follows
dBASE III where FoxPro does not.

A case must not QUIT, wait for a key or READ: it would stop the batch,
and the cases after it are reported as not run.  Cases share one FoxPro
session; between them the driver closes every table, releases every
variable and clears compiled programs, but SET options carry over, so a
case sets what it depends on.  Names are DOS names: 8.3, upper case back.

  FPW26=DIR   FoxPro 2.6 for Windows as installed: FOXPROW.EXE,
              FOXW2600.ESL, FOXTOOLS.FLL and the rest of the directory.
              FOXPROW.EXE and FOXW2600.ESL may be as shipped or already
              patched; as shipped, fpw26/*.gft patches them here (GRAFT).
              The patch is five bytes: the start-up calibration divides
              a loop count by 110, which overflows on any fast machine
              ("Divide By Zero or Overflow Error"); patched, the delay
              constant is the largest there is, on every host alike.
  WIN311=IMG  a Windows 3.11 hard disk (default ~/x86/disks/win311/c.img,
              ~/x86/tests/boot/wininstall.sh); copied, never changed
  DOSMON=PATH dos-monster (default ~/x86/dos-monster)
  GRAFT=PATH  graft, to patch a shipped FoxPro (default ~/graft/build/release/tools/graft)
  XBASE=PATH  for diff: the interpreter (default xbase/xbase.s32x)
  mtools      mcopy, mmd, mtype

  foxrun.py run CASE... [-o DIR] [-T SECS]    FoxPro's results in DIR/<case>
  foxrun.py diff CASE... [-o DIR] [-T SECS]   and xbase's in DIR/<case>.xbase

Exit 0 all ran (and agree), 1 any did not (or differ), 2 something it
needs is missing.
"""

import argparse, os, shutil, subprocess, sys, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, '..', '..', '..'))
PART = '@@32256'                       # the first partition, as the boot images have it
PATCHED = ('FOXPROW.EXE', 'FOXW2600.ESL')
DONE_COM = bytes.fromhex('B000E6F4CD20')   # mov al,0 / out 0F4h,al / int 20h
PATCH_AT = 0x10BC6                     # in both binaries: mov cx,110 / div cx ...
PATCH_OLD = bytes.fromhex('B96E00F7F1')
PATCH_NEW = bytes.fromhex('B8FEFF9090')  # ... becomes mov ax,0FFFEh / nop / nop


def mt(cmd, *args, check=True):
    env = dict(os.environ, MTOOLS_SKIP_CHECK='1')   # Windows leaves FATs mtools doubts
    return subprocess.run([cmd, *args], env=env, check=check, capture_output=True)


def verifies(graft, path, gft):
    """The patch was made from this very file (graft checks its hash)."""
    return subprocess.run([graft, 'verify', path, gft], capture_output=True).returncode == 0


def stage_foxpro(fpw26, graft, dest):
    """FoxPro's directory in dest, the two binaries patched; an error string or None."""
    os.makedirs(dest)
    for n in os.listdir(fpw26):
        p = os.path.join(fpw26, n)
        if os.path.isfile(p) and not n.lower().endswith('.orig'):
            shutil.copy(p, os.path.join(dest, n.upper()))
    for n in PATCHED:
        p = os.path.join(dest, n)
        if not os.path.exists(p):
            return '%s is not in FPW26' % n
        gft = os.path.join(HERE, 'fpw26', n + '.gft')
        if not (graft and os.access(graft, os.X_OK)):
            return 'graft (GRAFT) is needed to read %s' % gft
        if verifies(graft, p, gft):                 # as shipped
            subprocess.run([graft, 'apply', p, gft, p + '.new'], check=True, capture_output=True)
            os.replace(p + '.new', p)
            continue
        data = open(p, 'rb').read()                 # patched already: the shipped bytes put back verify
        if data[PATCH_AT:PATCH_AT + 5] == PATCH_NEW:
            open(p + '.was', 'wb').write(data[:PATCH_AT] + PATCH_OLD + data[PATCH_AT + 5:])
            ok = verifies(graft, p + '.was', gft)
            os.remove(p + '.was')
            if ok:
                continue
        return '%s is neither the shipped nor the patched one' % n
    return None


def as_text(v, t):
    """t = v as text whatever v's type, in short statements: dBASE III has no
    UDF, evaluates both arms of IIF() and takes 254 characters to a line."""
    return ['%s = "?"' % t,
            'IF TYPE("%s") = "N"' % v, '  %s = LTRIM(STR(%s))' % (t, v), 'ENDIF',
            'IF TYPE("%s") = "C"' % v, '  %s = %s' % (t, v), 'ENDIF']


# the two drivers' error log and markers in what both take: FWRITE (xbase has no FPUTS)
# and assignments (no =EXPR command in dBASE III)
PROCS = (['PROCEDURE errlog',
          'PARAMETERS e_n, e_msg, e_src, e_prog, e_line',
          'PRIVATE e_h, e_x, e_t1, e_t2, e_t3, e_t4, e_t5']
         + as_text('e_n', 'e_t1') + as_text('e_prog', 'e_t2') + as_text('e_line', 'e_t3')
         + as_text('e_msg', 'e_t4') + as_text('e_src', 'e_t5')
         + ['e_h = FOPEN(errfile, 2)',
            'IF e_h < 0',
            '  e_h = FCREATE(errfile)',
            'ENDIF',
            'e_x = FSEEK(e_h, 0, 2)',
            'e_x = FWRITE(e_h, "error " + e_t1 + " in " + e_t2 + " line " + e_t3 + ": " + e_t4 + CHR(13) + CHR(10))',
            'e_x = FWRITE(e_h, "  " + e_t5 + CHR(13) + CHR(10))',
            'e_x = FCLOSE(e_h)',
            'RETURN',
            '',
            'PROCEDURE marker',
            'PARAMETERS m_f',
            '=FCLOSE(FCREATE(m_f))',
            'RETURN'])
ON_ERROR = 'ON ERROR DO errlog WITH ERROR(), MESSAGE(), MESSAGE(1), PROGRAM(), LINENO()'


def driver(names):
    """MAIN.PRG: each case in turn, in FoxPro 2.6's language."""
    L = ['* foxrun.py: the cases in turn',
         'SET TALK OFF',
         'errfile = "C:\\CASES\\MAIN.ERR"',
         ON_ERROR,
         'SET SAFETY OFF',
         'SET CPDIALOG OFF']
    for i, _ in enumerate(names):
        d = 'C:\\CASES\\C%03d' % (i + 1)
        L += ['errfile = "%s\\ERR.TXT"' % d,
              'SET DEFAULT TO %s' % d,
              'SET ALTERNATE TO %s\\OUT.TXT' % d,
              'SET ALTERNATE ON',
              'SET CONSOLE OFF',
              'DO %s\\P.PRG' % d,
              'SET CONSOLE ON',
              'SET ALTERNATE OFF',
              'SET ALTERNATE TO',
              'CLOSE ALL',
              'CLEAR MEMORY',
              'CLEAR PROGRAM',
              'errfile = "C:\\CASES\\MAIN.ERR"',
              '=marker("%s\\RAN.TXT")' % d]
    L += ['=marker("C:\\CASES\\DONE.TXT")',
          'SET LIBRARY TO C:\\FPW26\\FOXTOOLS.FLL',
          '=CallFn(RegFn("ExitWindows", "LI", "I", "USER"), 0, 0)',
          'QUIT',
          '',
          ''] + PROCS
    return '\r\n'.join(L) + '\r\n'


def crlf(data):
    return data.replace(b'\r\n', b'\n').replace(b'\n', b'\r\n')


def run_foxpro(cases, work, fpw26, win311, dosmon, graft, secs):
    """Each case's results in work/<i>; per-case problems, or a batch error string."""
    err = stage_foxpro(fpw26, graft, os.path.join(work, 'FPW26'))
    if err:
        return None, err
    img = os.path.join(work, 'c.img')
    if subprocess.run(['cp', '-c', win311, img], capture_output=True).returncode:
        shutil.copy(win311, img)                    # no clonefile: an ordinary copy
    I = img + PART
    mt('mcopy', '-s', '-i', I, os.path.join(work, 'FPW26'), '::/')
    mt('mmd', '-i', I, '::/CASES')
    for i, c in enumerate(cases):
        cd = os.path.join(work, 'C%03d' % (i + 1))
        os.makedirs(cd)
        for n in os.listdir(c):
            p = os.path.join(c, n)
            if not os.path.isfile(p):
                continue
            data = open(p, 'rb').read()
            if n.upper().endswith('.PRG'):
                data = crlf(data)
            open(os.path.join(cd, n.upper()), 'wb').write(data)
        mt('mcopy', '-s', '-i', I, cd, '::/CASES/')
    open(os.path.join(work, 'MAIN.PRG'), 'wb').write(driver(cases).encode('latin-1'))
    open(os.path.join(work, 'DONE.COM'), 'wb').write(DONE_COM)
    auto = mt('mtype', '-i', I, '::/AUTOEXEC.BAT').stdout.decode('latin-1')
    keep = [l for l in auto.replace('\r', '').split('\n') if l.strip() and 'SMARTDRV' not in l.upper()]
    keep += ['win C:\\FPW26\\FOXPROW.EXE C:\\CASES\\MAIN.PRG', 'C:\\CASES\\DONE.COM']
    open(os.path.join(work, 'AUTOEXEC.BAT'), 'wb').write(('\r\n'.join(keep) + '\r\n').encode('latin-1'))
    for n in ('MAIN.PRG', 'DONE.COM'):
        mt('mcopy', '-i', I, os.path.join(work, n), '::/CASES/')
    mt('mcopy', '-o', '-i', I, os.path.join(work, 'AUTOEXEC.BAT'), '::/AUTOEXEC.BAT')

    log = open(os.path.join(work, 'dos-monster.log'), 'wb')
    subprocess.run([dosmon, '-m', '486', '-mem', '32', '-W', '-T', str(secs), '-hda', img, '-boot', 'c'],
                   stdin=subprocess.DEVNULL, stdout=log, stderr=log)
    log.close()

    back = os.path.join(work, 'back')
    os.makedirs(back)
    mt('mcopy', '-s', '-n', '-i', I, '::/CASES', back, check=False)
    cb = os.path.join(back, 'CASES')
    done = os.path.exists(os.path.join(cb, 'DONE.TXT'))
    main_err = os.path.join(cb, 'MAIN.ERR')
    probs = []
    for i in range(len(cases)):
        d = os.path.join(cb, 'C%03d' % (i + 1))
        p = []
        if not os.path.exists(os.path.join(d, 'RAN.TXT')):
            p.append(('did not run to its end', True))
        elif os.path.exists(os.path.join(d, 'ERR.TXT')):
            n = errors(os.path.join(d, 'ERR.TXT'))
            p.append(('%d error%s (ERR.TXT)' % (n, '' if n == 1 else 's'), False))   # what FoxPro does, not a failure
        probs.append(p)
    batch = None
    if not done:
        batch = 'FoxPro did not finish in %d s' % secs
    if os.path.exists(main_err):
        batch = (batch + '; ' if batch else '') + 'driver errors: ' + open(main_err, 'rb').read().decode('latin-1').strip()
    return (cb, probs), batch


def run_xbase(case, od, xbase, emu):
    """The case under xbase, in the same frame: errors logged and passed over."""
    shutil.copytree(case, od)
    L = ['SET TALK OFF', 'SET SAFETY OFF', 'errfile = "ERR.TXT"', ON_ERROR,
         'SET ALTERNATE TO OUT.TXT', 'SET ALTERNATE ON', 'SET CONSOLE OFF', 'DO P',
         'SET CONSOLE ON', 'SET ALTERNATE OFF', 'SET ALTERNATE TO', 'CLOSE ALL',
         'ON ERROR', 'e_x = FCLOSE(FCREATE("RAN.TXT"))', 'RETURN']
    with open(os.path.join(od, 'W.PRG'), 'w', newline='') as f:
        f.write('\n'.join(L) + '\n')
    # dBASE III finds a procedure in its own file, not further down the caller
    with open(os.path.join(od, 'ERRLOG.PRG'), 'w', newline='') as f:
        f.write('\n'.join(PROCS[1:PROCS.index('RETURN') + 1]) + '\n')
    with open(os.path.join(od, 'xbase.log'), 'wb') as log:
        subprocess.run([emu, xbase], input=b'DO W\nQUIT\n', stdout=log, stderr=log, cwd=od)


def errors(path):
    """How many errors a driver logged."""
    if not os.path.exists(path):
        return 0
    return sum(1 for l in open(path, 'rb') if l.startswith(b'error '))


def text(path):
    if not os.path.exists(path):
        return None
    return open(path, 'rb').read().rstrip(b'\x1a').replace(b'\r\n', b'\n')


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest='cmd', required=True)
    for k in ('run', 'diff'):
        a = sub.add_parser(k)
        a.add_argument('cases', nargs='+')
        a.add_argument('-o')
        a.add_argument('-T', type=int, default=600)
    args = ap.parse_args()
    fpw26 = os.environ.get('FPW26', '')
    win311 = os.environ.get('WIN311', os.path.expanduser('~/x86/disks/win311/c.img'))
    dosmon = os.environ.get('DOSMON', os.path.expanduser('~/x86/dos-monster'))
    graft = os.environ.get('GRAFT', os.path.expanduser('~/graft/build/release/tools/graft'))
    xbase = os.environ.get('XBASE', os.path.join(ROOT, 'xbase', 'xbase.s32x'))
    emu = os.path.join(ROOT, 'tools', 'dbt', 'slow32-dbt')
    need = [('FPW26 (a FoxPro 2.6 for Windows directory)', fpw26 and os.path.exists(os.path.join(fpw26, 'FOXPROW.EXE'))),
            ('WIN311 (a Windows 3.11 disk image)', os.path.exists(win311)),
            ('dos-monster (DOSMON)', os.access(dosmon, os.X_OK)),
            ('mtools', shutil.which('mcopy') and shutil.which('mmd'))]
    if args.cmd == 'diff':
        need += [('xbase.s32x (XBASE)', os.path.exists(xbase)), ('tools/dbt/slow32-dbt', os.access(emu, os.X_OK))]
    missing = [w for w, ok in need if not ok]
    bad_cases = [c for c in args.cases if not os.path.exists(os.path.join(c, 'P.PRG'))]
    if missing or bad_cases:
        if missing:
            print('foxrun: missing ' + ', '.join(missing), file=sys.stderr)
        for c in bad_cases:
            print('foxrun: %s has no P.PRG' % c, file=sys.stderr)
        return 2
    names, seen = [], set()
    for c in args.cases:
        n = os.path.basename(os.path.normpath(c))
        while n in seen:
            n += '_'
        seen.add(n)
        names.append(n)

    out = args.o or tempfile.mkdtemp(prefix='foxrun.')
    os.makedirs(out, exist_ok=True)
    work = tempfile.mkdtemp(prefix='work.', dir=out)
    res, batch = run_foxpro(args.cases, work, fpw26, win311, dosmon, graft, args.T)
    if res is None:
        print('foxrun: ' + batch, file=sys.stderr)
        shutil.rmtree(work)
        return 2
    cb, probs = res
    fails = 0
    for i, (c, n) in enumerate(zip(args.cases, names)):
        d = os.path.join(out, n)
        if os.path.exists(d):
            shutil.rmtree(d)
        src = os.path.join(cb, 'C%03d' % (i + 1))
        if os.path.isdir(src):
            shutil.copytree(src, d)
        else:
            os.makedirs(d)
        p = list(probs[i])
        if args.cmd == 'diff':
            xd = d + '.xbase'
            if os.path.exists(xd):
                shutil.rmtree(xd)
            run_xbase(c, xd, xbase, emu)
            if not os.path.exists(os.path.join(xd, 'RAN.TXT')):
                p.append(('xbase did not run to its end (%s)' % os.path.join(xd, 'xbase.log'), True))
            ne, nf = errors(os.path.join(xd, 'ERR.TXT')), errors(os.path.join(d, 'ERR.TXT'))
            if ne:
                p.append(('xbase %d error%s' % (ne, '' if ne == 1 else 's'), ne != nf))
            a, b = text(os.path.join(d, 'OUT.TXT')), text(os.path.join(xd, 'OUT.TXT'))
            if b is None:
                p.append(('xbase wrote no OUT.TXT', True))
            elif a != b:
                p.append(('OUT.TXT differs (diff %s %s)' % (os.path.join(d, 'OUT.TXT'), os.path.join(xd, 'OUT.TXT')), True))
        if any(bad for _, bad in p):
            fails += 1
        print('%s: %s' % (n, '; '.join(m for m, _ in p) if p else 'ok'))
    if batch:
        print('foxrun: ' + batch)
        fails = fails or 1
    shutil.rmtree(work)
    print('foxrun: %d case%s, %d not ok; results in %s' % (len(names), '' if len(names) == 1 else 's', fails, out))
    return 1 if fails else 0


if __name__ == '__main__':
    sys.exit(main())
