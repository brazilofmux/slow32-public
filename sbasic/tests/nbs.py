#!/usr/bin/env python3
"""The NBS Minimal BASIC test programs (NBS SP 500-70) on SLOW BASIC,
held to QBasic 1.1 -- the QuickBASIC interpreter, run on dos-monster.

    sbasic/tests/nbs.py [-v] [-j N] [--dir DIR] [P001 P056 ...]  (default: all)

With --dir, any directory of BASIC programs (*.bas or *.BAS, each with
an optional sibling .in or .input of keys) is run the same way: e.g.
the BASIC Computer Games of the same collection,
~/refs/basic/nbs-ecma55-test/basic_computer_games (their .output files
are another interpreter's, and are not used).

The suite is NOT in this tree.  It is the 208 programs of the
8bitworkshop transcription (github.com/sehugg/nbs-ecma55-test, NBS/),
fetched to ~/refs/basic/nbs-ecma55-test (or NBS=dir).  Each program
checks itself as it goes ("*** TEST PASSED ***", "TEST FAILED"), and some
are exception tests meant to stop.

The oracle is QBasic 1.1, C:\\DOS\\QBASIC.EXE on the MS-DOS 6.22 image
of ~/x86 (QBASIC=path to a copy, or X86=the x86 tree to take it from),
run headless by dos-monster's program runner, its output read back with
-transcript (every line that scrolls off, then the last screen).  For
QBasic the program is changed in one way: END and STOP become SYSTEM, so
QBasic leaves instead of returning to its editor.  A program's sibling
.in is its keyboard (sbasic reads it as stdin; QBasic gets it as typed
keys, lines ended by CR).  QBasic's outputs are cached by content in
~/.cache/sbasic-oracle.

Verdicts, per program:
  SAME        sbasic prints what QBasic prints (trailing blanks aside)
  SAME-ERROR(m) both stop on the error m (QBasic's output before its
              dialog is not compared: the dialog is over it)
  BOTH-REJECT(m) both refuse the program's syntax, in their own words
  DIFF(n)     n lines differ
  QB-ERROR(m) QBasic stopped on an error, its dialog's message m: a
              program it refuses on loading, or a run-time error
  SB-TIMEOUT  sbasic did not finish; QB-TIMEOUT likewise QBasic
The outputs and a unified diff of each are kept in $OUT (default
/tmp/sbasic-nbs), with results.tsv.
"""
import concurrent.futures as cf
import difflib
import hashlib
import os
import re
import shutil
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, '../..'))
NBS = os.environ.get('NBS', os.path.expanduser('~/refs/basic/nbs-ecma55-test/NBS'))
X86 = os.environ.get('X86', os.path.expanduser('~/x86'))
EMU = os.environ.get('EMU', os.path.join(ROOT, 'tools/emulator/slow32-fast'))
SBASIC = os.path.join(ROOT, 'sbasic/sbasic.s32x')
OUT = os.environ.get('OUT', '/tmp/sbasic-nbs')
CACHE = os.path.expanduser('~/.cache/sbasic-oracle')
TLIMIT = int(os.environ.get('TIMEOUT', '30'))


def qbasic_exe():
    """A copy of QBASIC.EXE: $QBASIC, or taken from the MS-DOS 6.22 image."""
    if os.environ.get('QBASIC'):
        return os.environ['QBASIC']
    dst = os.path.join(CACHE, 'QBASIC.EXE')
    if not os.path.exists(dst):
        os.makedirs(CACHE, exist_ok=True)
        img = os.path.join(X86, 'disks/msdos622/c.img')
        subprocess.run(['mcopy', '-n', '-o', '-i', img + '@@32256', '::/DOS/QBASIC.EXE', dst], check=True)
    return dst


def clean(text):
    """lines, trailing blanks trimmed, trailing empty lines dropped"""
    lines = [l.rstrip() for l in text.replace('\r\n', '\n').split('\n')]
    while lines and not lines[-1]:
        lines.pop()
    return lines


def run_sbasic(src, inp):
    try:
        r = subprocess.run([EMU, '-q', SBASIC, src], input=inp, capture_output=True, timeout=TLIMIT)
    except subprocess.TimeoutExpired:
        return None
    out = r.stdout.decode('latin-1')
    lines = clean(out)
    if lines and lines[0].startswith('Loaded ') and ' lines from ' in lines[0]:
        lines = lines[1:]
    return lines


END_RE = re.compile(rb'^(\s*\d+\s+)(END|STOP)\s*$', re.I)


def for_qbasic(src):
    """the program as QBasic is given it: CRLF, END and STOP as SYSTEM"""
    lines = src.replace(b'\r\n', b'\n').rstrip(b'\n').split(b'\n')
    out = []
    for l in lines:
        m = END_RE.match(l)
        out.append(m.group(1) + b'SYSTEM' if m else l)
    return b'\r\n'.join(out) + b'\r\n'


# what may stand before a program's first executable statement in QBasic:
# DECLARE, COMMON, DEFtype, $ metacommands, comments, blank lines
PROLOGUE_RE = re.compile(rb"^\s*(\d+\s+)?(DECLARE\b|COMMON\b|DEF(INT|LNG|SNG|DBL|STR)\b|REM\b|'|$)", re.I)


def load_probe(src):
    """src with SYSTEM as its first executable statement: QBasic, given
    it, ends at once if it loads the program, and shows the load-time
    error if not.  (SYSTEM before a DECLARE or COMMON would itself be
    the error: "COMMON and DECLARE must precede executable statements".)"""
    lines = src.replace(b'\r\n', b'\n').split(b'\n')
    k = 0
    while k < len(lines) and PROLOGUE_RE.match(lines[k]):
        k += 1
    return b'\r\n'.join(lines[:k] + [b'SYSTEM'] + lines[k:])


def dialog_message(screen):
    """QBasic's error dialog on the final screen: the text inside its box
    above the <  OK  > row, or None when there is no dialog"""
    rows = screen.split('\n')
    ok = [i for i, r in enumerate(rows) if '<  OK  >' in r]
    if not ok:
        return None
    i = ok[0]
    # the dialog's own left edge: the column of the border before "<  OK  >"
    left = rows[i].rfind('\u2502', 0, rows[i].find('<  OK  >'))
    right = rows[i].find('\u2502', rows[i].find('<  OK  >'))
    msg = []
    j = i - 1
    while j >= 0 and left >= 0 and right > left:
        seg = rows[j][left:right + 1] if len(rows[j]) > right else ''
        if not seg or seg[0] not in '\u2502\u251c\u250c':
            break
        if seg[0] == '\u250c':
            break
        if seg[0] == '\u2502':
            t = seg[1:-1].strip()
            if t:
                msg.insert(0, t)
        j = j - 1
    return ' '.join(msg) or '?'


def qb_lines(data):
    """QBasic's output, without the "Press any key to continue" it adds when
    a program ends at an END that is not on a numbered line of its own"""
    lines = clean(data)
    if lines and lines[-1] == 'Press any key to continue':
        lines.pop()
        while lines and not lines[-1]:
            lines.pop()
    return lines


def run_qbasic(src, inp, exe, fresh=False):
    """QBasic's output for src with keys inp, or its verdict (ERROR(msg),
    TIMEOUT); cached, unless fresh -- a TIMEOUT can be a run cut off
    before its screen was dumped, worth asking again"""
    prog = for_qbasic(src)
    keys = inp.replace(b'\r\n', b'\n').replace(b'\n', b'\r')
    key = hashlib.sha256(prog + b'\0' + keys).hexdigest()
    cached = os.path.join(CACHE, 'qb-' + key + '.txt')
    if os.path.exists(cached) and not fresh:
        data = open(cached, 'rb').read().decode('utf-8')
        if data.startswith('\0'):
            return data[1:]
        return qb_lines(data)
    d = tempfile.mkdtemp(prefix='sbnbs-')
    try:
        shutil.copy(exe, os.path.join(d, 'QBASIC.EXE'))
        open(os.path.join(d, 'PROG.BAS'), 'wb').write(prog)
        tr = os.path.join(d, 'out.txt')
        scr = os.path.join(d, 'screen.txt')
        try:
            subprocess.run([os.path.join(X86, 'dos-monster'), '-m', '386', '-W', '-C', d,
                            '-transcript', tr, '-D', scr, '-T', str(TLIMIT), 'QBASIC.EXE', '/RUN', 'PROG.BAS'],
                           input=keys, capture_output=True, timeout=TLIMIT + 30, cwd=d)
        except subprocess.TimeoutExpired:
            pass
        data = open(tr, 'rb').read().decode('utf-8') if os.path.exists(tr) else ''
        screen = open(scr, 'rb').read().decode('utf-8') if os.path.exists(scr) else ''
        # a run cut off by -T leaves QBasic's screen: an error dialog, its
        # editor, or a program still running
        verdict = None
        if '<  OK  >' in screen:
            verdict = 'ERROR(' + dialog_message(screen) + ')'
        elif not data or 'File  Edit  View' in screen:
            verdict = 'TIMEOUT'
        os.makedirs(CACHE, exist_ok=True)
        open(cached, 'wb').write(('\0' + verdict if verdict else data).encode('utf-8'))
        return verdict if verdict else qb_lines(data)
    finally:
        shutil.rmtree(d, ignore_errors=True)


def program_path(name):
    for ext in ('.BAS', '.bas'):
        if os.path.exists(os.path.join(NBS, name + ext)):
            return os.path.join(NBS, name + ext)
    return os.path.join(NBS, name + '.BAS')


def one(name, exe):
    src = program_path(name)
    inp = b''
    for ext in ('.in', '.input'):
        if os.path.exists(os.path.join(NBS, name + ext)):
            inp = open(os.path.join(NBS, name + ext), 'rb').read()
            break
    sb = run_sbasic(src, inp)
    qb = run_qbasic(open(src, 'rb').read(), inp, exe)
    open(os.path.join(OUT, name + '.sb'), 'w').write('\n'.join(sb or ['<timeout>']) + '\n')
    open(os.path.join(OUT, name + '.qb'), 'w').write(('<' + qb + '>' if isinstance(qb, str) else '\n'.join(qb)) + '\n')
    if sb is None:
        return name, 'SB-TIMEOUT', 0
    if isinstance(qb, str):
        # QBasic stopped on an error: does sbasic stop on the same one?
        # (QBasic's output screen is gone under its dialog, so only the
        # message is compared)
        if qb.startswith('ERROR(') and sb:
            msg = qb[len('ERROR('):-1]
            last = re.sub(r'^Error in line \d+: ', 'Error: ', sb[-1])
            last = re.sub(r' \(.*\)$', '', last)
            if last.endswith('Error: ' + msg):
                return name, 'SAME-ERROR(' + msg + ')', 0
            # both refuse the program as written: QBasic when it loads it
            # (its "Expected: ..." and syntax messages), sbasic when it
            # parses it ("Error in line N: ...") -- in their own words
            if (msg.startswith('Expected') or msg == 'Syntax error') and \
                    re.match(r'Error in line \d+: ', sb[-1]):
                return name, 'BOTH-REJECT(' + msg + ')', 0
        return name, 'QB-' + qb, 0
    # a program left waiting for more keys than its .in has: QBasic sits at
    # the prompt (its cursor, an arrow, ends the transcript) and sbasic
    # stops with "Input past end"; compare what came before
    if qb and qb[-1].endswith('\u2192'):
        qb = qb[:-1] + [qb[-1][:-1].rstrip()]
    if sb and sb[-1] == 'Error: Input past end':
        sb = sb[:-1]
    if sb == qb:
        return name, 'SAME', 0
    diff = list(difflib.unified_diff(qb, sb, 'qbasic', 'sbasic', lineterm='', n=1))
    open(os.path.join(OUT, name + '.diff'), 'w').write('\n'.join(diff) + '\n')
    n = sum(1 for l in diff if l.startswith('-') and not l.startswith('---'))
    return name, 'DIFF(%d)' % n, n


def main():
    global NBS, OUT
    args = sys.argv[1:]
    if '--dir' in args:
        i = args.index('--dir')
        NBS = os.path.abspath(os.path.expanduser(args[i + 1]))
        del args[i:i + 2]
        if 'OUT' not in os.environ:
            OUT = '/tmp/sbasic-' + os.path.basename(NBS.rstrip('/'))
    verbose = '-v' in args
    jobs = 8
    if '-j' in args:
        jobs = int(args[args.index('-j') + 1])
        del args[args.index('-j'):args.index('-j') + 2]
    names = [a for a in args if not a.startswith('-')]
    if not names:
        names = sorted(f[:-4] for f in os.listdir(NBS) if f.upper().endswith('.BAS'))
    os.makedirs(OUT, exist_ok=True)
    exe = qbasic_exe()
    with cf.ThreadPoolExecutor(max_workers=jobs) as ex:
        res = list(ex.map(lambda n: one(n, exe), names))
    counts = {}
    with open(os.path.join(OUT, 'results.tsv'), 'w') as o:
        for name, verdict, _ in res:
            o.write('%s\t%s\n' % (name, verdict))
            kind = verdict.split('(')[0]
            counts[kind] = counts.get(kind, 0) + 1
            if verbose or kind != 'SAME':
                print('%-6s %s' % (name, verdict))
    print('  '.join('%s %d' % (k, counts[k]) for k in sorted(counts)), '(of %d)' % len(res))


if __name__ == '__main__':
    main()
