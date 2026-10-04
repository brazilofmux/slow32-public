#!/bin/bash
# scredit-differential.sh [-N] [-n COUNT] [-s SEED] -- the editor core against
# Micro Focus's ADIS: the same picture, initial value and keys through
# tests/scredit_test (the core, on the host) and tests/adischeck.sh (MS
# COBOL 5 under the DOS translator); the field and the cursor after every
# key, and the item at the end, must be the same.  First the fixed cases
# below, then COUNT random key strings (default 60) from SEED (default 1).
# Needs the oracle rig (~/x86 with MS COBOL 5: see adischeck.sh); about
# 0.6 s a case.  Text fields (2026-10-04: seeds 2 3 7 11 19 at -n 200
# all agree; seed 23 has two cases that differ, both Ctrl-A then
# Backspace in a one-column field).
#
# -N: numeric fields instead (tests/scrnum_test): the cases of
# tests/scrnum.txt, then random ones over some thirty picture shapes.
# For a picture with suppression and no point (ZZ9) only the item is
# compared unless FULL=1: ADIS draws such a field one position to the
# left while the cursor is past its digits, and with an insertion
# character in it (ZZZ,ZZ9) not through the picture, which the driver
# does not imitate.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
ST=${SCREDIT_TEST:-}
AC=${ADISCHECK:-$HERE/adischeck.sh}
count=60; seed=1; numeric=0
while [ $# -gt 0 ]; do case "$1" in -N) numeric=1; shift;; -n) count=$2; shift 2;; -s) seed=$2; shift 2;; *) echo "what is $1?" >&2; exit 2;; esac; done
W="$(mktemp -d "${TMPDIR:-/tmp}/scrdiff.XXXXXX")"
trap 'case "$W" in */scrdiff.*) rm -rf -- "$W";; esac' EXIT
if [ -z "$ST" ] && [ $numeric = 1 ]; then
    ST="$W/scrnum_test"
    "${HOSTCC:-cc}" -std=gnu99 -O1 -w -I"$HERE/../libcob" -I"$HERE/../src" -o "$ST" "$HERE/scrnum_test.c" "$HERE/../src/picture.c" "$HERE/../src/picture_scan.c" || { echo "scredit-differential: host build failed" >&2; exit 2; }
elif [ -z "$ST" ]; then
    ST="$W/scredit_test"
    "${HOSTCC:-cc}" -std=gnu99 -O1 -w -I"$HERE/../libcob" -o "$ST" "$HERE/scredit_test.c" || { echo "scredit-differential: host build failed" >&2; exit 2; }
fi
if [ $numeric = 1 ]; then
grep -v '^#' "$HERE/scrnum.txt" | grep -v '^N:' > "$W/cases.txt"      # N: cases are natural entry: ours, no oracle
python3 - "$count" "$seed" >> "$W/cases.txt" <<'PY'
import random, sys
count, seed = int(sys.argv[1]), int(sys.argv[2])
rnd = random.Random(seed)
pics = '''ZZZ99.99 ZZ9.99 999.99 9(5) Z,ZZ9.99 -ZZ9.99 ZZ9.99- $$,$$9.99 ***9.99 ZZZ.ZZ 99/99/99 9(3)V99 ZZ9.99CR +ZZ9.99
          ---9.99 Z9 $ZZ9.99 99.9 ZZZ,ZZ9 9(3) ZZ9 +++9 ZZ,ZZ9.99 9(4)V9 ZZ9.9 --,--9.99 $$$9 Z(5) ---9 **9'''.split()
keys = list('0123456789') * 3 + ['.'] * 5 + ['-', '+', '{LEFT}', '{LEFT}', '{LEFT}', '{RIGHT}', '{RIGHT}', '{BS}', '{BS}', '{BS}',
        '{DEL}', '{DEL}', '{^X}', '{^Z}', '{^A}', '{END}', '{HOME}']
vals = ['', '', '2.5', '7', '12']                     # values every picture here can hold
for _ in range(count):
    pic = rnd.choice(pics)
    val = rnd.choice(vals)
    if '.' not in pic and 'V' not in pic: val = val.split('.')[0]
    print('%s~%s~%s' % (pic, val, ''.join(rnd.choice(keys) for _ in range(rnd.randint(3, 12)))))
PY
else
python3 - "$count" "$seed" > "$W/cases.txt" <<'PY'
import random, sys
count, seed = int(sys.argv[1]), int(sys.argv[2])
fixed = [
 ('X(6)', '', 'abc{LEFT}{LEFT}Z{END}w'), ('X(6)', '', 'abcdefgh'), ('X(6)', '', 'abcd{LEFT}{LEFT}{INS}XY{INS}Q'),
 ('X(6)', '', 'abcdef{HOME}{INS}XY'), ('X(6)', '', 'abcd{BS}{BS}xy{BS}'), ('X(6)', '', 'abcd{LEFT}{LEFT}{LEFT}{DEL}{DEL}{^R}'),
 ('X(6)', 'hello', '{RIGHT}{RIGHT}{^Z}{^A}{^X}Q'), ('X(6)', 'hello', 'ab{BS}{BS}'), ('X(6)', 'hello', '{RIGHT}{^F}{^O}'),
 ('A(4)', '', 'a1b-c d'),
]
rnd = random.Random(seed)
keys = list('abcXYZ12 ') * 3 + ['{LEFT}'] * 4 + ['{RIGHT}'] * 3 + ['{BS}'] * 4 + ['{DEL}'] * 3 + ['{INS}'] * 2 + \
       ['{^X}', '{^Z}', '{^A}', '{^O}', '{^R}', '{^F}', '{END}', '{HOME}']
pics = [('X(6)', ['', 'hello', 'ab']), ('X(3)', ['', 'xyz']), ('A(4)', ['', 'ab']), ('X(1)', ['', 'q'])]
for _ in range(count):
    pic, vals = rnd.choice(pics)
    fixed.append((pic, rnd.choice(vals), ''.join(rnd.choice(keys) for _ in range(rnd.randint(4, 14)))))
for pic, val, k in fixed: print('%s~%s~%s' % (pic, val, k))
PY
fi
pass=0; fail=0
while IFS='~' read -r pic val keys; do
    "$ST" "$pic" "$val" "$keys{ENTER}" > "$W/ours.txt" 2>&1
    if [ -n "$val" ] && [ $numeric = 1 ]; then "$AC" -v "$val" "$pic" "$keys{ENTER}" > "$W/raw.txt" 2>&1
    elif [ -n "$val" ]; then "$AC" -v "\"$val\"" "$pic" "$keys{ENTER}" > "$W/raw.txt" 2>&1; else "$AC" "$pic" "$keys{ENTER}" > "$W/raw.txt" 2>&1; fi
    python3 - "$W/raw.txt" > "$W/theirs.txt" <<'PY'
import re, sys
out = []
for l in open(sys.argv[1], encoding='utf-8', errors='replace').read().split('\n')[2:]:
    if l.startswith('ENTER'): continue
    if l.startswith(' ' * 9 + 'ITEM=['): out.append(l.strip()); continue
    m = re.match(r'^(.{8}) \[(.*?)\]  (\S+)', l)
    if m: out.append('%-8s [%s]  %s' % (m.group(1).rstrip() or ' ', m.group(2), m.group(3)))
print('\n'.join(out))
PY
    if [ $numeric = 1 ] && [ "${FULL:-}" != 1 ]; then
        case "$pic" in *.*|*V*) ;; *Z*|*'*'*|*++*|*--*|*'$$'*)
            tail -1 "$W/ours.txt" > "$W/o1"; tail -1 "$W/theirs.txt" > "$W/t1"; mv "$W/o1" "$W/ours.txt"; mv "$W/t1" "$W/theirs.txt" ;;
        esac
    fi
    if diff -q "$W/ours.txt" "$W/theirs.txt" >/dev/null; then pass=$((pass+1))
    else
        fail=$((fail+1))
        echo "== DIFFERS: $pic  value [$val]  keys $keys"
        diff "$W/theirs.txt" "$W/ours.txt" | sed 's/^</  adis </; s/^>/  ours >/' | head -8
    fi
done < "$W/cases.txt"
echo "scredit differential: $pass agree, $fail differ"
[ $fail -eq 0 ]
