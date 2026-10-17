#!/bin/bash

# The kit's islisp.s32x, run on every engine over checked-in programs.
#
# run-kit-differential.sh and run-kit-tools-differential.sh exercise the
# kit's compiler and tools; the applications the kit ships beside them were
# copied into slow32:kit and never run.  This runs the shipped islisp.s32x
# over islisp/tests/*.lsp and requires each engine to print exactly the
# committed output.  It needs neither the oracle image nor the verify suite
# (that lives outside the tree; islisp/tests/run-tests.sh is the full gate).
#
#   <name>.expected       what the oracle (Easy-ISLisp) prints for <name>.lsp;
#                         islisp/tests/run-tests.sh checks it against the
#                         oracle whenever the oracle image is present.
#   <name>.repl.expected  the same program fed to the REPL on stdin, prompts
#                         and echoed values included.  qemu's -kernel passes
#                         no argv, so this is how qemu runs it; the other
#                         engines run both forms.  islisp's own behaviour,
#                         pinned, not an oracle's.  REGEN_REPL=1 rewrites
#                         these from slow32-fast after a deliberate change.
#
# lists.lsp allocates about two million conses with a few thousand live; the
# islisp of 2026-10-16 ran out of its 32MB heap on it under SLOW-32's malloc.
#
# Usage: ./run-kit-islisp.sh [test-name ...]
# Env:   KIT (default ~/s32x), ISLISP (default $KIT/islisp.s32x), SLOW32,
#        SLOW32_FAST, SLOW32_DBT, QEMU_S32, TIMEOUT (secs, default 120).
# Exit:  0 every engine printed the expected output, 1 a mismatch, 2 setup.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SLOW32_BASE="$(cd "$SCRIPT_DIR/.." && pwd)"

SLOW32="${SLOW32:-$SLOW32_BASE/tools/emulator/slow32}"
SLOW32_FAST="${SLOW32_FAST:-$SLOW32_BASE/tools/emulator/slow32-fast}"
SLOW32_DBT="${SLOW32_DBT:-$SLOW32_BASE/tools/dbt/slow32-dbt}"
QEMU_S32="${QEMU_S32:-$HOME/qemu/build/qemu-system-slow32}"
KIT="${KIT:-$HOME/s32x}"
ISLISP="${ISLISP:-$KIT/islisp.s32x}"
TESTS="$SLOW32_BASE/islisp/tests"
TIMEOUT="${TIMEOUT:-120}"

if [ ! -f "$ISLISP" ]; then
    echo "ERROR: no islisp at $ISLISP (set KIT or ISLISP)" >&2
    exit 2
fi
ISLISP="$(cd "$(dirname "$ISLISP")" && pwd)/$(basename "$ISLISP")"   # runs cd into the tests

# The same filter as run-kit-differential.sh: engine banners and statistics.
# The REPL's last prompt has no newline, so whatever the engine prints next
# lands on its line; the awk splits it off first.
normalize_output() {
    awk '/^> (Starting execution|MMIO enabled|HALT at|Program halted|Exit code:|Instructions executed|slow32: )/ {
             print "> "; print substr($0, 3); next } { print }' | \
    sed \
        -e 's/\r$//' \
        -e '/^Starting execution/d' \
        -e '/^MMIO enabled/d' \
        -e '/^HALT at/d' \
        -e '/^Program halted/d' \
        -e '/^Exit code:/d' \
        -e '/^Instructions executed/d' \
        -e '/^Simulated cycles:/d' \
        -e '/^Wall time:/d' \
        -e '/^Performance:/d' \
        -e '/^Cycles:/d' \
        -e '/instructions\/second/d' \
        -e '/^slow32: native intrinsics detected:/d' \
        -e '/^slow32: math intercepts:/d' \
        -e '/^  mem[a-z]*:  *0x[0-9a-f]*$/d' \
        -e '/^  strlen:  *0x[0-9a-f]*$/d' \
    | awk '
        { lines[NR] = $0; if ($0 ~ /[^[:space:]]/) last = NR }
        END { for (i = 1; i <= last; i++) print lines[i] }
    '
}

# name|path|takes-argv
ENGINES=()
for spec in "slow32|$SLOW32|1" "slow32-fast|$SLOW32_FAST|1" \
            "slow32-dbt|$SLOW32_DBT|1" "qemu|$QEMU_S32|0"; do
    path="$(echo "$spec" | cut -d'|' -f2)"
    if [ -x "$path" ]; then ENGINES+=("$spec")
    else echo "  SKIPPED ENGINE: ${spec%%|*} ($path) -- not available"; fi
done
[ ${#ENGINES[@]} -gt 0 ] || { echo "ERROR: no engines" >&2; exit 2; }

W="$(mktemp -d "${TMPDIR:-/tmp}/kit-islisp.XXXXXX")"
trap 'rm -rf "${W:?}"' EXIT

if [ $# -gt 0 ]; then NAMES=("$@")
else NAMES=(); for f in "$TESTS"/*.expected; do
    case "$f" in *.repl.expected) continue ;; esac
    [ -e "$f" ] && NAMES+=("$(basename "$f" .expected)"); done
fi

echo "islisp:  $ISLISP"
echo "tests:   $TESTS (${#NAMES[@]})"

if [ "${REGEN_REPL:-0}" = 1 ]; then
    for b in "${NAMES[@]}"; do
        (cd "$TESTS" && timeout "$TIMEOUT" "$SLOW32_FAST" "$ISLISP" < "$b.lsp" 2>&1) \
            | normalize_output > "$TESTS/$b.repl.expected"
        echo "  wrote $b.repl.expected"
    done
    exit 0
fi

PASS=0
FAIL=0
check() {   # label got-file rc expected-file
    if [ "$3" -ne 0 ]; then
        printf "  %-28s FAIL (exit %s)\n" "$1" "$3"; FAIL=$((FAIL+1))
    elif ! cmp -s "$4" "$2"; then
        printf "  %-28s FAIL (output differs)\n" "$1"; diff "$4" "$2" | head -6; FAIL=$((FAIL+1))
    else
        printf "  %-28s PASS\n" "$1"; PASS=$((PASS+1))
    fi
}

for b in "${NAMES[@]}"; do
    [ -f "$TESTS/$b.lsp" ] && [ -f "$TESTS/$b.expected" ] && [ -f "$TESTS/$b.repl.expected" ] \
        || { echo "ERROR: $b needs $b.lsp, $b.expected and $b.repl.expected" >&2; exit 2; }
    for spec in "${ENGINES[@]}"; do
        name="${spec%%|*}"; path="$(echo "$spec" | cut -d'|' -f2)"; argv="${spec##*|}"
        if [ "$argv" = 1 ]; then
            rc=0
            (cd "$TESTS" && timeout "$TIMEOUT" "$path" "$ISLISP" "$b.lsp" </dev/null >"$W/raw" 2>&1) || rc=$?
            normalize_output < "$W/raw" > "$W/got"
            check "$b $name" "$W/got" "$rc" "$TESTS/$b.expected"
        fi
        rc=0
        if [ "$name" = qemu ]; then
            (cd "$TESTS" && timeout "$TIMEOUT" "$path" -machine slow32-tcg -nographic -monitor none \
                -kernel "$ISLISP" < "$b.lsp" >"$W/raw" 2>&1) || rc=$?
        else
            (cd "$TESTS" && timeout "$TIMEOUT" "$path" "$ISLISP" < "$b.lsp" >"$W/raw" 2>&1) || rc=$?
        fi
        normalize_output < "$W/raw" > "$W/got"
        check "$b $name (repl)" "$W/got" "$rc" "$TESTS/$b.repl.expected"
    done
done

echo "kit islisp: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ] || exit 1
exit 0
