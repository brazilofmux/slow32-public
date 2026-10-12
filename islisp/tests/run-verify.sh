#!/bin/bash
# The ISLISP verification suite (Easy-ISLisp's verify/, a port of the
# ISLisp verification system) through our interpreter, with the oracle's
# own run of each file as the reference.  Each file announces its
# chapters and prints one "... is bad. correct is X but got Y" line per
# failed check; a file passes when it runs to completion with no more
# such lines than the oracle itself printed (the oracle has two, both in
# misc.lsp's clock functions).
#
#   tests/run-verify.sh [--host] [--s32x] [--stage08] [FILE...]
#
# --host runs out/islisp-host (the default: fast); --s32x runs
# out/islisp.s32x on the emulator; --stage08 runs out/islisp-stage08.s32x.
# FILE names select verify files (list, number, object ...).  The suite
# lives in ~/refs/eisl/verify (VERIFY=... to point elsewhere), never in
# the tree.  Oracle outputs are cached under $OUT/oracle.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
IDIR="$(cd "$HERE/.." && pwd)"
ROOT="$(cd "$IDIR/.." && pwd)"
VERIFY="${VERIFY:-$HOME/refs/eisl/verify}"
EISL_ROOT="$(cd "$VERIFY/.." && pwd)"
OUT="${OUT:-/tmp/islisp-verify}"
EMU="${EMU:-$ROOT/tools/dbt/slow32-dbt}"
mkdir -p "$OUT/oracle" "$OUT/ours"

# The files that exercise the standard; the rest are the oracle's own
# extensions (its libraries, Unicode, threads, the stress programs).
INSCOPE="list pred cond symbol macro block unwind formeval declare control number bignum string char seq array vector object ilos io stream files misc"

MODE=host
FILES=""
while [ $# -gt 0 ]; do
    case "$1" in
        --host) MODE=host ;;
        --s32x) MODE=s32x ;;
        --stage08) MODE=stage08 ;;
        *) FILES="$FILES $1" ;;
    esac
    shift
done
[ -n "$FILES" ] || FILES="$INSCOPE"

run_ours() {   # file -> output on stdout, exit status
    local f="$1"
    case "$MODE" in
        host) (cd "$EISL_ROOT" && timeout 600 "$IDIR/out/islisp-host" -l "$IDIR/lib" "verify/$f.lsp" 2>&1) ;;
        s32x) (cd "$EISL_ROOT" && timeout 600 "$EMU" "$IDIR/out/islisp.s32x" -l "$IDIR/lib" "verify/$f.lsp" 2>&1) ;;
        stage08) (cd "$EISL_ROOT" && timeout 600 "$EMU" "$IDIR/out/islisp-stage08.s32x" -l "$IDIR/lib" "verify/$f.lsp" 2>&1) ;;
    esac
}

oracle_bad() {   # cached count of the oracle's own failures
    local f="$1"
    if [ ! -f "$OUT/oracle/$f.out" ]; then
        if "$HERE/oracle.sh" --check 2>/dev/null; then
            (cd "$EISL_ROOT" && podman run --rm -v "$EISL_ROOT:/work" -w /work slow32:islisp-oracle "verify/$f.lsp" > "$OUT/oracle/$f.out" 2>&1)
        else
            echo 0; return
        fi
    fi
    grep -c "is bad" "$OUT/oracle/$f.out"
}

PASS=0; FAIL=0
for f in $FILES; do
    [ -f "$VERIFY/$f.lsp" ] || { printf "  %-10s MISSING\n" "$f"; continue; }
    out="$(run_ours "$f")"; rc=$?
    echo "$out" > "$OUT/ours/$f.out"
    # Lines the suite expects and the text does not: tests/verify-exceptions.txt
    # lists them as "file<TAB>form prefix<TAB>reason"; they are not counted.
    # (a prefix beginning "re:" is an extended regular expression instead)
    bad=$(echo "$out" | grep "is bad" \
          | grep -v -F -f <(awk -F'\t' -v f="$f" '$1 == f && $2 !~ /^re:/ { print $2 }' "$HERE/verify-exceptions.txt"; echo "@@none@@") \
          | grep -v -E -f <(awk -F'\t' -v f="$f" '$1 == f && $2 ~ /^re:/ { print substr($2, 4) }' "$HERE/verify-exceptions.txt"; echo "@@none@@") \
          | grep -c "is bad")
    obad=$(oracle_bad "$f")
    checks=$(grep -c '(\$test\|(\$error\|(\$assert' "$VERIFY/$f.lsp")
    # Agreement is necessary, not sufficient: a run that printed nothing
    # at all (a wrong emulator path, a silent exit) agrees with everything.
    headers=$(grep -c '(\$ap ' "$VERIFY/$f.lsp")
    seen=$(echo "$out" | grep -c '^[0-9] ')
    if [ "$rc" -eq 0 ] && [ "$headers" -gt 0 ] && [ "$seen" -eq 0 ]; then
        printf "  %-10s VACUOUS (no output; %s chapter headers expected)\n" "$f" "$headers"
        FAIL=$((FAIL+1))
        continue
    fi
    if [ "$rc" -ne 0 ]; then
        last="$(echo "$out" | grep -v '^[0-9] ' | tail -1 | cut -c1-70)"
        printf "  %-10s STOPPED rc=%s bad=%s/%s  %s\n" "$f" "$rc" "$bad" "$checks" "$last"
        FAIL=$((FAIL+1))
    elif [ "$bad" -gt "$obad" ]; then
        printf "  %-10s FAIL(%s of %s)  oracle %s\n" "$f" "$bad" "$checks" "$obad"
        FAIL=$((FAIL+1))
    else
        printf "  %-10s PASS (%s checks)\n" "$f" "$checks"
        PASS=$((PASS+1))
    fi
done
echo "islisp verify ($MODE): $PASS pass, $FAIL fail; outputs in $OUT"
[ "$FAIL" -eq 0 ]
