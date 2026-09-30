#!/bin/bash
# nist-sql-run.sh -- the NIST SQL Test Suite, Version 6.0, embedded COBOL
# (docs/esql.md), as ccvs-run.sh runs CCVS-85: load the schemas, then every
# program runpco.all lists, in its order and under its authorization id,
# against one set of SQLite databases, and total the tests that print
# "*** pass ***" and "*** fail ***".
#
#   NISTSQL=~/refs/nist-sql     the unpacked suite (never in a git tree)
#   NSQL_KEEP=1                 keep the work directory
#   NSQL_ONLY=dml001            one program (after the schemas and the
#                               data loaders), its output shown
#
# Not run: the mpa/mpb pairs (they must run concurrently; one SQLite
# connection has no locking) and the one C program.  They are counted.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"; C="$HERE/.."; ROOT="$C/.."
NIST="${NISTSQL:-$HOME/refs/nist-sql}"
EMU=${EMU:-$ROOT/tools/dbt/slow32-dbt}
[ -x "$EMU" ] || EMU=$ROOT/tools/emulator/slow32-fast
[ -d "$NIST/pco" ] || { echo "no NIST SQL suite at $NIST (set NISTSQL)" >&2; exit 1; }
W="$(mktemp -d "${TMPDIR:-/tmp}/nistsql.XXXXXX")"
[ -n "${NSQL_KEEP:-}" ] && echo "work directory: $W" >&2 || trap 'rm -rf "$W"' EXIT
mkdir -p "$W/db" "$W/src" "$W/run"

# the schemas: the standard set, the first time each file is named
refused=0
while read -r f u; do
    r=$(python3 "$HERE/nist-sql-schema.py" "$W/db" "$NIST/schema/$f" "$u" | tail -1 | sed 's/.*: \([0-9]*\) refused/\1/')
    refused=$((refused + r))
done < <(tr -d '\r' < "$NIST/schema/runsch.all" | awk '($1=="RUNSCH"||$1=="RUNSQL") && !seen[$2]++ {print $2, $3}' | head -19)

progs=0; comp=0; ran=0; pass=0; fail=0; nocomp=0; crash=0; skipped=0
while read -r kind name auth; do
    case "$kind" in
    RUNSQL) [ -f "$NIST/sql/$name.sql" ] && python3 "$HERE/nist-sql-schema.py" "$W/db" "$NIST/sql/$name.sql" "$auth" >/dev/null
            continue ;;                         # report and dataload have no SQL file in the suite
    RUNPCO_2|RUNC) skipped=$((skipped + 1)); continue ;;
    esac
    progs=$((progs + 1))
    loader=0; case "$name" in basetab|cugtab|flattab|suntab*|sultab1|cts5tab|predml) loader=1 ;; esac
    [ -n "${NSQL_ONLY:-}" ] && [ "$name" != "$NSQL_ONLY" ] && [ $loader = 0 ] && continue
    srcs=("$W/src/$name.cbl")
    tr -d '\r' < "$NIST/pco/$name.pco" > "$W/src/$name.cbl"
    subs=()
    case "$kind" in
    RUNPCO_S) subs=("${name}s") ;;
    RUNPCO_T) subs=("${name}s" "${name}t") ;;
    esac
    for s in "${subs[@]+"${subs[@]}"}"; do tr -d '\r' < "$NIST/pco/$s.pco" > "$W/src/$s.cbl"; srcs+=("$W/src/$s.cbl"); done
    if ! "$C/compile.sh" -fixed "${srcs[@]}" -o "$W/run/$name.s32x" > "$W/run/$name.cc" 2>&1; then
        nocomp=$((nocomp + 1))
        echo "$name: does not compile: $(grep -m1 'error' "$W/run/$name.cc" | sed 's/.*error: //')"
        continue
    fi
    comp=$((comp + 1))
    ( cd "$W/run" && COB_SQL_DIR="$W/db" COB_SQL_USER="$auth" timeout 120 "$EMU" "$name.s32x" < /dev/null > "$name.out" 2> "$name.err" ); rc=$?
    p=$(grep -c '\*\*\* pass \*\*\*' "$W/run/$name.out"); f=$(grep -c '\*\*\* fail \*\*\*' "$W/run/$name.out")
    pass=$((pass + p)); fail=$((fail + f))
    if [ $rc -ne 0 ] && [ $rc -ne 1 ]; then crash=$((crash + 1)); echo "$name: stopped (status $rc) after $p pass, $f fail: $(tail -1 "$W/run/$name.err")"
    else ran=$((ran + 1)); echo "$name: $p pass, $f fail"; fi      # every program: a quiet regression shows in a diff
    if [ -n "${NSQL_ONLY:-}" ] && [ "$name" = "$NSQL_ONLY" ]; then cat "$W/run/$name.out" "$W/run/$name.err"; fi
done < <(tr -d '\r' < "$NIST/pco/runpco.all" | awk '$1 ~ /^RUN/ {print $1, $2, $3}')

echo "NIST SQL embedded COBOL: $progs programs, $comp compile, $ran run to the end; tests $pass pass, $fail fail; $nocomp do not compile, $crash stop early, $skipped not run (concurrent pairs, C); $refused schema elements refused"
