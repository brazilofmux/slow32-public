#!/bin/bash
# pic-differential.sh -- PICTURE validity, this compiler against GnuCOBOL.
#
# Every picture of one to three symbols over 9 Z * + - $ . V P S B 0 / ,
# CR DB, and 3,000 random ones of four to seven (a fixed seed): 7,368
# pictures, each compiled here under -std=85 and all of them at once by
# GnuCOBOL 4 (-std=cobol85, podman image gnucobol:4.0-builder), whose
# errors carry line numbers.  Prints how many each refuses and lists the
# pictures they disagree on.
#
# The 55 disagreements of 2026-09-29 were each settled by the text, and
# the text sides with this compiler in all of them
# (docs/conformance/picture.md, docs/oracles.md): any other count is a
# change to look at.  Exit 0 when the count is the known one.
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
COBC="${S32_COBC:-$HERE/../out/s32-cobc}"
ENGINE="${ORACLE_ENGINE:-$(command -v podman || command -v docker)}"
KNOWN=55
W="$(mktemp -d "$HERE/../out/picdiff.XXXXXX")"
trap 'rm -rf "$W"' EXIT
cd "$W"

python3 - <<'EOF'
import itertools, random
A = ['9','Z','*','+','-','$','.','V','P','S','B','0','/',',','CR','DB']
pics = set()
for L in (1, 2, 3):
    for t in itertools.product(A, repeat=L): pics.add(''.join(t))
random.seed(7)
while len(pics) < 4368 + 3000:
    pics.add(''.join(random.choice(A) for _ in range(random.randint(4, 7))))
pics = sorted(pics)
open('pics.txt', 'w').write('\n'.join(pics) + '\n')
with open('all.cbl', 'w') as o:
    o.write('identification division.\nprogram-id. pd.\ndata division.\nworking-storage section.\n')
    for i, p in enumerate(pics): o.write(f'01 i{i} pic {p}.\n')
    o.write('procedure division.\n    stop run.\n')
EOF

"$ENGINE" run --rm -v "$W:$W" -w "$W" gnucobol:4.0-builder \
    cobc -fsyntax-only -free -std=cobol85 -fmax-errors=100000 all.cbl > gc.log 2>&1 || true

: > ours.txt
while IFS= read -r p; do
    printf 'identification division.\nprogram-id. pd.\ndata division.\nworking-storage section.\n01 i pic %s.\nprocedure division.\n    stop run.\n' "$p" > one.cbl
    if "$COBC" -free -std=85 -o one.s one.cbl >/dev/null 2>&1; then echo ok >> ours.txt; else echo err >> ours.txt; fi
done < pics.txt

python3 - "$KNOWN" <<'EOF'
import re, sys
pics = open('pics.txt').read().split('\n')[:-1]
ours = open('ours.txt').read().split()
bad = set()
for l in open('gc.log'):
    m = re.match(r'all\.cbl:(\d+): error', l)
    if m: bad.add(int(m.group(1)) - 5)
we = [p for i, p in enumerate(pics) if ours[i] == 'err' and i not in bad]
gc = [p for i, p in enumerate(pics) if ours[i] == 'ok' and i in bad]
print(f"{len(pics)} pictures: we refuse {ours.count('err')}, GnuCOBOL {len(bad)}")
print(f"we refuse, GnuCOBOL accepts ({len(we)}): {' '.join(we)}")
print(f"GnuCOBOL refuses, we accept ({len(gc)}): {' '.join(gc)}")
n = len(we) + len(gc)
print(f"{n} disagreements, {sys.argv[1]} known")
sys.exit(0 if n == int(sys.argv[1]) else 1)
EOF
