#!/bin/bash
# prof.sh [compile.sh flags] prog.cbl [more sources] -- where a program's
# instructions go: built against a libcob whose every function is
# labelled, run under the reference interpreter with -p, and symbolized
# by prof.py.  The program runs in a work directory of its own (files it
# writes land there); PROF_KEEP=1 keeps it, TOP=n lists n symbols.
#
#   bench/prof.sh -free bench/vs/karith.cbl
#   TOP=20 bench/prof.sh -free -std=2002 prog.cbl sub.cbl
#
# The profile counts guest instructions.  Under the DBT the hooked
# kernels and mem* run natively, so prof.py reports them apart and gives
# the split of what is left ("guest-only").  For wall time, run the
# .s32x under tools/dbt/slow32-dbt, with -H to see what the hooks cover.
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"; C="$HERE/.."; ROOT="$C/.."
W="$(mktemp -d)"; [ -n "${PROF_KEEP:-}" ] && echo "work: $W" >&2 || trap 'rm -rf "$W"' EXIT
[ -f "$C/libcob/libcob.s" ] || "$C/build.sh" >/dev/null
# libcob with a global alias beside every function, static ones included
python3 - "$C/libcob/libcob.s" "$W" <<'PY'
import re, sys
lines = open(sys.argv[1]).read().split('\n')
funcs = set(m.group(1) for l in lines for m in [re.match(r'\s*\.type\s+([\w.$]+),@function', l)] if m)
out = []
for l in lines:
    out.append(l)
    m = re.match(r'^([\w.$]+):', l)
    if m and m.group(1) in funcs:
        n = m.group(1).replace('.', '_'); out.append('\t.globl __p_%s\n__p_%s:' % (n, n))
open(sys.argv[2] + '/libcob-p.s', 'w').write('\n'.join(out))
open(sys.argv[2] + '/libcob.names', 'w').write('\n'.join(sorted(f.replace('.', '_') for f in funcs)))
PY
"$ROOT/tools/assembler/slow32asm" "$W/libcob-p.s" "$W/libcob-p.s32o" >/dev/null
srcs=(); for a in "$@"; do case "$a" in *.cbl|*.cob|*.CBL|*.COB) srcs+=("$a") ;; esac; done
[ ${#srcs[@]} -gt 0 ] || { echo "usage: prof.sh [flags] prog.cbl [sources...]" >&2; exit 2; }
gen=$(cat "${srcs[@]}" | grep -ai 'program-id\|function-id' | sed -E 's/.*-[iI][dD]\.? *([A-Za-z0-9_-]+).*/\1/' | tr 'A-Z-' 'a-z_' | sort -u | tr '\n' ' ')
S32_LIBCOB="$W/libcob-p.s32o" "$C/compile.sh" "$@" -o "$W/prog.s32x" >/dev/null
(cd "$W" && "$ROOT/tools/emulator/slow32" -p prog.prof prog.s32x > prog.out 2>&1) || true
python3 "$HERE/prof.py" "$W/prog.s32x" "$W/prog.prof" "$W/libcob.names" $gen main -n "${TOP:-12}"
