#!/bin/bash
# adischeck.sh -- what Micro Focus's ADIS does with each key in a screen
# field: the oracle for docs/plans/screen-input.md.
#
#   tests/adischeck.sh [-c CLAUSES] [-i ITEMPIC] [-v VALUE] [-2] PICTURE KEYS
#
# A one-field SCREEN SECTION program (the field at line 2, column 6,
# PICTURE as given, USING an item of ITEMPIC -- default: the picture's
# own digits or X(n) -- with VALUE) is compiled by Microsoft COBOL 5.0
# (Micro Focus underneath, 1993; ~/x86/disks/cobol50), linked with ADIS,
# and run under the DOS translator with its key trace (-K): each key is
# released when the program is waiting for it, and the screen and cursor
# are written before it.  Printed: the field and the cursor after each
# key, then the item when the ACCEPT ends.
#
#   -c CLAUSES   more clauses on the screen item (AUTO, FULL, REQUIRED, ...)
#   -2           a second field (PIC X(4), line 4) after it, to see leaving
#   KEYS         characters, with {ENTER} {TAB} {BTAB} {BS} {DEL} {INS}
#                {LEFT} {RIGHT} {UP} {DOWN} {HOME} {END} {ESC} {F1}..{F10}
#                and {^X} for a control key
#
#   tests/adischeck.sh 'ZZZ99.99' '1234{BS}5678{ENTER}'
#   tests/adischeck.sh -i 'X(8)' -v '"AB"' 'X(8)' 'xy{LEFT}{INS}q{ENTER}'
#
# The cursor column is counted from the field's first column (1); a
# cursor elsewhere prints as (row,col).  shape is the cursor's scan
# lines when they change (an insert-mode cursor is a different shape).
set -eu
X86=${X86:-$HOME/x86}
DM="$X86/dos-monster"
[ -x "$DM" ] && [ -f "$X86/disks/cobol50/COBOL.EXE" ] || { echo "adischeck: no $DM or $X86/disks/cobol50" >&2; exit 2; }
"$DM" -h 2>&1 | grep -q -- "-K FILE" || { echo "adischeck: $DM has no key trace (-K)" >&2; exit 2; }
clauses=""; itempic=""; value=""; second=0
while [ $# -gt 2 ]; do
    case "$1" in
        -c) clauses="$2"; shift 2 ;;
        -i) itempic="$2"; shift 2 ;;
        -v) value="$2"; shift 2 ;;
        -2) second=1; shift ;;
        *) echo "adischeck: what is $1?" >&2; exit 2 ;;
    esac
done
[ $# -eq 2 ] || { sed -n 2,26p "$0"; exit 2; }
M="$(mktemp -d "${TMPDIR:-/tmp}/adischeck.XXXXXX")"
trap 'case "$M" in */adischeck.*) rm -rf -- "$M";; esac' EXIT
cp -R "$X86/disks/cobol50/." "$M/"
python3 - "$M" "$1" "$2" "$clauses" "$itempic" "$value" "$second" <<'PY'
import re, sys
M, pic, keys, clauses, itempic, value, second = sys.argv[1:8]
def expand(p):
    return re.sub(r'(.)\((\d+)\)', lambda m: m.group(1) * int(m.group(2)), p.upper())
ex = expand(pic)
numeric = not re.search(r'[XAN]', ex)
if not itempic:
    if numeric:
        body = ex.split('CR')[0].split('DB')[0]
        point = body.find('.') if '.' in body else body.find('V') if 'V' in body else len(body)
        digs = lambda t: sum(1 for ch in t if ch in '9Z*') + max(0, sum(1 for ch in t if ch in '$+-') - 1)
        ni, nf = digs(body[:point]), digs(body[point:])
        signed = bool(re.search(r'[S+\-]|CR|DB', ex))
        itempic = ('S' if signed else '') + '9(%d)' % max(ni, 1) + ('V9(%d)' % nf if nf else '')
    else:
        itempic = 'X(%d)' % len(ex)
if not value:
    value = '0' if not re.search(r'[XAN]', expand(itempic)) else 'SPACES'
L = ['$SET ANS85',
     ' IDENTIFICATION DIVISION.', ' PROGRAM-ID. A1.',
     ' ENVIRONMENT DIVISION.', ' CONFIGURATION SECTION.', ' SPECIAL-NAMES.',
     '     CRT STATUS IS CS.',
     ' DATA DIVISION.', ' WORKING-STORAGE SECTION.',
     ' 01  ITEM PIC %s VALUE %s.' % (itempic, value),
     ' 01  F2 PIC X(4) VALUE SPACES.',
     ' 01  CS.', '     03  CS1 PIC X.', '     03  CS2 PIC 99 COMP-X.', '     03  CS3 PIC 99 COMP-X.',
     ' 01  CSD.', '     03  D1 PIC X.', '     03  FILLER PIC X VALUE "/".', '     03  D2 PIC 999.',
     '     03  FILLER PIC X VALUE "/".', '     03  D3 PIC 999.',
     ' SCREEN SECTION.', ' 01  S1.',
     '     03  LINE 2 COL 6 PIC %s USING ITEM' % pic,
     '         %s.' % clauses if clauses else None]
if L[-1] is None: L[-2] += '.'; L.pop()
if second == '1': L += ['     03  LINE 4 COL 6 PIC X(4) USING F2.']
L += [' PROCEDURE DIVISION.', '     DISPLAY S1.', '     ACCEPT S1.',
      '     MOVE CS1 TO D1 MOVE CS2 TO D2 MOVE CS3 TO D3.',
      '     DISPLAY "ITEM=[" AT 0801 ITEM "]".',
      '     DISPLAY "F2=[" AT 0901 F2 "] CRT=" CSD.',
      '     STOP RUN.']
open(M + '/A1.CBL', 'w', newline='').write('\r\n'.join('      ' + l for l in L) + '\r\n')
NAMES = {'ENTER': '\r', 'TAB': '\t', 'BTAB': '\x1b[Z', 'BS': '\x08', 'DEL': '\x1b[3~', 'INS': '\x1b[2~',
         'LEFT': '\x1b[D', 'RIGHT': '\x1b[C', 'UP': '\x1b[A', 'DOWN': '\x1b[B', 'HOME': '\x1b[H', 'END': '\x1b[F',
         'CEND': '\x1b[1;5F', 'CHOME': '\x1b[1;5H', 'ESC': '\x1b', 'F1': '\x1bOP', 'F2': '\x1bOQ', 'F3': '\x1bOR', 'F4': '\x1bOS', 'F5': '\x1b[15~',
         'F6': '\x1b[17~', 'F7': '\x1b[18~', 'F8': '\x1b[19~', 'F9': '\x1b[20~', 'F10': '\x1b[21~'}
out = []; labels = []
for m in re.finditer(r'\{(\^?)([A-Za-z0-9]+)\}|(.)', keys):
    if m.group(3) is not None: out.append(m.group(3)); labels.append(m.group(3))
    elif m.group(1): out.append(chr(ord(m.group(2).upper()) & 0x1F)); labels.append('^' + m.group(2).upper())
    else: out.append(NAMES[m.group(2).upper()]); labels.append(m.group(2).upper())
open(M + '/keys.bin', 'wb').write(''.join(out).encode('latin-1'))
open(M + '/labels.txt', 'w').write('\n'.join(labels) + '\n')
open(M + '/width.txt', 'w').write(str(len(ex.replace('V', '').replace('S', '')) + (0 if 'CR' not in ex and 'DB' not in ex else 0)))
PY
if ! "$DM" -C "$M" -L 4000000000 "$M/COBOL.EXE" "A1;" </dev/null 2>&1 | tr -d '\r' | grep -q "no errors"; then
    echo "adischeck: MS COBOL 5 refused the program:" >&2
    "$DM" -C "$M" -L 4000000000 "$M/COBOL.EXE" "A1;" </dev/null 2>&1 | tr -d '\r' | grep -v "^$" | tail -8 >&2
    exit 1
fi
"$DM" -C "$M" -L 4000000000 "$M/LINK.EXE" "A1+ADIS+ADISINIT+ADISKEY,,,lcobol+cobapi/nod/st:8192;" </dev/null >/dev/null 2>&1
exe=$(ls "$M" | grep -i '^a1\.exe$' | head -1)
[ -n "$exe" ] || { echo "adischeck: link failed" >&2; exit 1; }
"$DM" -C "$M" -T 20 -K "$M/kt.txt" "$M/$exe" < "$M/keys.bin" >/dev/null 2>&1 || true
python3 - "$M" "$1" <<'PY'
import sys
M, pic = sys.argv[1:3]
labels = open(M + '/labels.txt').read().split('\n')[:-1]
w = int(open(M + '/width.txt').read())
frames = []; cur = None
for line in open(M + '/kt.txt', encoding='utf-8', errors='replace').read().split('\n'):
    if line.startswith('== '):
        p = line.split(); cur = {'n': int(p[1]), 'row': int(p[3]), 'col': int(p[4]), 'shape': (p[6], p[7]), 'scr': [], 'next': None}
        frames.append(cur)
    elif line.startswith('-- ') and cur is not None: cur['next'] = line[3:]; cur = None
    elif cur is not None: cur['scr'].append(line)
print('PIC %s   (field at line 2, columns 6-%d)' % (pic, 5 + w))
print('%-8s %-*s  %s' % ('key', max(w, 5) + 2, 'field', 'cursor'))
shape = None
for f in frames:
    scr = f['scr'] + [''] * 25
    fld = scr[1].ljust(80)[5:5 + w]
    key = '(start)' if f['n'] == 0 else labels[f['n'] - 1] if f['n'] - 1 < len(labels) else '?'
    where = str(f['col'] - 5) if f['row'] == 2 and 6 <= f['col'] <= 5 + w + 1 else '(%d,%d)' % (f['row'], f['col'])
    extra = ''
    if f['shape'] != shape: extra = '   shape %s-%s' % f['shape'] if shape is not None else ''; shape = f['shape']
    other = [l for i, l in enumerate(scr[:25]) if i != 1 and l.strip() and i < 7]
    print('%-8s [%s]  %-7s%s%s' % (key, fld, where, extra, ('   | ' + ' | '.join(o.strip() for o in other)) if other else ''))
    if f['next'] in ('exit', 'end'):
        for l in scr[7:9]:
            if l.strip(): print('         ' + l.rstrip())
        if f['next'] == 'end': print('         (keys ran out; the ACCEPT was still waiting)')
PY
