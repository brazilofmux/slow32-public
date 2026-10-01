#!/bin/bash
# cpmcheck.sh TEST... -- run free-format tests (tests/free/NAME.cbl) under
# Microsoft MS-COBOL 4.65 (1982, CP/M-80), a 74-era witness: a subset of
# X3.23-1974: no 85 syntax, no REMAINDER, no ON SIZE ERROR, and a
# multi-phrase INSPECT REPLACING stops its compiler.  Each test is converted to fixed format (comments dropped,
# words outside literals uppercased, code from column 8, long lines
# wrapped outside literals), compiled, linked (L80) and run under the Z80
# project's CP/M machine (~/z80/z80-monster, MS-COBOL from
# ~/z80/disks/mscobol), and its screen is printed beside our output.
# Where it departs from the 85 text, read the 74 text before calling it
# a defect: docs/oracles.md keeps the readings.
#
# A PATH names a program outside tests/free (its .expected beside it);
# a fixed-format one, as in tests/witness, is taken as written;
# KEEP=1 keeps the work directory.
#
#   Z80=~/z80 tests/cpmcheck.sh /path/to/prog74.cbl
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
Z80=${Z80:-$HOME/z80}
ZM="$Z80/z80-monster"; MS="$Z80/disks/mscobol"
[ -x "$ZM" ] && [ -f "$MS/COBOL.COM" ] || { echo "cpmcheck: no $ZM or $MS" >&2; exit 2; }
M="$(mktemp -d "${TMPDIR:-/tmp}/cpmcheck.XXXXXX")"
[ -n "${KEEP:-}" ] && echo "kept $M" >&2 || trap 'case "$M" in */cpmcheck.*) rm -rf -- "$M";; esac' EXIT
for name in "$@"; do
    case "$name" in
        */*) src="$name"; name=$(basename "$name" .cbl); exp="${src%.cbl}.expected" ;;
        *) src="$HERE/free/$name.cbl"; exp="$HERE/free/$name.expected" ;;
    esac
    [ -f "$src" ] || { echo "== $name: no $src"; continue; }
    up=$(echo "$name" | cut -c1-8 | tr 'a-z' 'A-Z')
    python3 - "$src" "$M/$up.COB" "$up" <<'PY'
import sys, re
def upper(l):
    out, q = [], None
    for ch in l:
        if q:
            if ch == q: q = None
            out.append(ch)
        else:
            if ch in "\"'": q = ch
            out.append(ch.upper())
    return "".join(out)
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
out = []
para = False
src = open(sys.argv[1]).read().splitlines()
if all(l.startswith("       ") or not l.strip() for l in src):
    # already fixed format (tests/witness): as written, in uppercase
    out = [upper(l.rstrip()) for l in src if l.strip()]
    src = []
for l in src:
    if l.lstrip().startswith("*>"): continue
    l = re.sub(r"\s\*>.*$", "", l).rstrip()
    if not l: continue
    l = upper(l)
    l = re.sub(r"^(\s*PROGRAM-ID\.\s*)\S+", r"\g<1>" + sys.argv[3] + ".", l)
    if para and not re.match(r"^[A-Z0-9-]+( SECTION)?\.$", l.strip()):
        out.append("       P0.")         # 74 wants a paragraph before the first statement
    para = l.strip() == "PROCEDURE DIVISION."
    out += ["       " + p for p in wrap(l.strip() if not l.startswith(" ") else l)]
open(sys.argv[2], "w", newline="").write("\r\n".join(out) + "\r\n\x1a")
PY
    printf '@wait-idle\nCOBOL %s,%s=%s\\r\n@wait-idle\nL80 %s,%s/N/E\\r\n@wait-idle\n%s\\r\n@wait-idle\n@dump %s/out.dump\n@end\n' \
        "$up" "$up" "$up" "$up" "$up" "$up" "$M" > "$M/go.script"
    case "$M/$up.dsk" in */cpmcheck.*) rm -f -- "$M/$up.dsk";; esac
    (cd "$Z80" && ./tools/mkdsk -f hd "$M/$up.dsk" "$MS"/COBOL.COM "$MS"/COBOL[1-4].OVR "$MS"/L80.COM \
        "$MS"/COBLIB.REL "$MS"/COBLBX.REL "$MS"/CRTDRV.REL "$MS"/COBLOC "$MS"/RUNCOB.COM "$M/$up.COB" >/dev/null)
    (cd "$Z80" && timeout 300 ./z80-monster -j --script "$M/go.script" -A "$M/$up.dsk" >/dev/null 2>&1) || true
    (cd "$Z80" && ./tools/mkdsk -x "$M/$up.dsk" "$up.PRN" -o "$M" >/dev/null 2>&1) || true
    echo "== $name: MS-COBOL 4.65"
    [ -f "$M/$up.PRN" ] && tr -d '\r\032' < "$M/$up.PRN" | grep -E "^[0-9]{4}: " | sed 's/^/   diag /' || true
    sed 's/ *$//' "$M/out.dump" | grep -E "^\?|Error|Underflow" | sed 's/^/   screen /' || true
    # the screen after the program's own command line, up to the next prompt
    sed 's/ *$//' "$M/out.dump" | awk -v c="A>$up" '$0==c{on=1;next} /^A>/{on=0} on && NF' | sed 's/^/   /'
    echo "== $name: ours"
    [ -f "$exp" ] && sed 's/^/   /' "$exp" || echo "   (no $exp)"
done
