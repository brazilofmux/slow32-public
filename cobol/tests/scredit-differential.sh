#!/bin/bash
# scredit-differential.sh [-n COUNT] [-s SEED] -- the editor core against
# Micro Focus's ADIS: the same picture, initial value and keys through
# tests/scredit_test (the core, on the host) and tests/adischeck.sh (MS
# COBOL 5 under the DOS translator); the field and the cursor after every
# key, and the item at the end, must be the same.  First the fixed cases
# below, then COUNT random key strings (default 60) from SEED (default 1).
# Needs the oracle rig (~/x86 with MS COBOL 5: see adischeck.sh); about
# 0.6 s a case.  Text fields so far (2026-10-04: seeds 2 3 7 11 19 at
# -n 200 all agree; seed 23 has two cases that differ, both Ctrl-A then
# Backspace in a one-column field).
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
ST=${SCREDIT_TEST:-}
AC=${ADISCHECK:-$HERE/adischeck.sh}
count=60; seed=1
while [ $# -gt 0 ]; do case "$1" in -n) count=$2; shift 2;; -s) seed=$2; shift 2;; *) echo "what is $1?" >&2; exit 2;; esac; done
W="$(mktemp -d "${TMPDIR:-/tmp}/scrdiff.XXXXXX")"
trap 'case "$W" in */scrdiff.*) rm -rf -- "$W";; esac' EXIT
if [ -z "$ST" ]; then
    ST="$W/scredit_test"
    "${HOSTCC:-cc}" -std=gnu99 -O1 -w -I"$HERE/../libcob" -o "$ST" "$HERE/scredit_test.c" || { echo "scredit-differential: host build failed" >&2; exit 2; }
fi
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
pass=0; fail=0
while IFS='~' read -r pic val keys; do
    "$ST" "$pic" "$val" "$keys{ENTER}" > "$W/ours.txt" 2>&1
    if [ -n "$val" ]; then "$AC" -v "\"$val\"" "$pic" "$keys{ENTER}" > "$W/raw.txt" 2>&1; else "$AC" "$pic" "$keys{ENTER}" > "$W/raw.txt" 2>&1; fi
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
    if diff -q "$W/ours.txt" "$W/theirs.txt" >/dev/null; then pass=$((pass+1))
    else
        fail=$((fail+1))
        echo "== DIFFERS: $pic  value [$val]  keys $keys"
        diff "$W/theirs.txt" "$W/ours.txt" | sed 's/^</  adis </; s/^>/  ours >/' | head -8
    fi
done < "$W/cases.txt"
echo "scredit differential: $pass agree, $fail differ"
[ $fail -eq 0 ]
