#!/bin/bash
# mfcheck.sh TEST... -- run free-format COBOL 85 tests (tests/free/NAME.cbl)
# under Microsoft COBOL 5.0 (Micro Focus underneath, 1993), a second
# witness beside GnuCOBOL (docs/oracles.md).  Each test is converted to
# fixed format ($SET ANS85, comments dropped, code from column 8, long
# lines wrapped outside literals), compiled, linked and run under the
# x86 project's DOS translator (~/x86/dos-monster, MS COBOL 5 from
# ~/x86/disks/cobol50, as its tests/dos/run.sh does), and its output is
# printed beside ours.  MS COBOL DISPLAYs a numeric item without the
# point and with a trailing sign, so numeric lines are compared by digits
# and sign by eye; edited and alphanumeric lines compare byte for byte.
#
#   X86=~/x86 tests/mfcheck.sh divremu inspord editins
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
X86=${X86:-$HOME/x86}
DM="$X86/dos-monster"
[ -x "$DM" ] && [ -f "$X86/disks/cobol50/COBOL.EXE" ] || { echo "mfcheck: no $DM or $X86/disks/cobol50" >&2; exit 2; }
M="$(mktemp -d "${TMPDIR:-/tmp}/mfcheck.XXXXXX")"
trap 'case "$M" in */mfcheck.*) rm -rf -- "$M";; esac' EXIT
cp -R "$X86/disks/cobol50/." "$M/"
for name in "$@"; do
    src="$HERE/free/$name.cbl"
    [ -f "$src" ] || { echo "== $name: no $src"; continue; }
    short=$(echo "$name" | cut -c1-8)
    python3 - "$src" "$M/$(echo "$short" | tr 'a-z' 'A-Z').CBL" <<'PY'
import sys, re
def wrap(l, width=64):
    out = []
    while len(l) > width:
        q = False; cut = -1
        for i, ch in enumerate(l[:width]):
            if ch == '"': q = not q
            if ch == ' ' and not q: cut = i
        if cut <= 4: break
        out.append(l[:cut]); l = "    " + l[cut + 1:].lstrip()
    out.append(l)
    return out
out = ["      $SET ANS85"]
for l in open(sys.argv[1]).read().splitlines():
    if l.lstrip().startswith("*>"): continue
    l = re.sub(r"\s\*>.*$", "", l).rstrip()
    if l:
        out += ["       " + p for p in wrap(l)]
open(sys.argv[2], "w", newline="").write("\r\n".join(out) + "\r\n")
PY
    up=$(echo "$short" | tr 'a-z' 'A-Z')
    if ! "$DM" -C "$M" -L 4000000000 "$M/COBOL.EXE" "$up;" </dev/null 2>&1 | tr -d '\r' | grep -q "no errors"; then
        echo "== $name: MS COBOL 5 refused it"; continue
    fi
    "$DM" -C "$M" -L 4000000000 "$M/LINK.EXE" "$short,,,lcobol+cobapi/nod/st:8192;" </dev/null >/dev/null 2>&1
    exe=$(ls "$M" | grep -i "^$short\.exe$" | head -1)
    echo "== $name: MS COBOL 5 | ours"
    paste -d'|' <("$DM" -C "$M" -L 20000000000 "$M/$exe" </dev/null 2>/dev/null | tr -d '\r') "$HERE/free/$name.expected"
done
