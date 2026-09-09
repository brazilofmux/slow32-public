#!/bin/bash
# Acceptance test for the stage08 self-hosted compiler: build SQLite twice,
# once with clang and once with stage08, and require the two programs to
# print the SAME BYTES.  clang is the oracle; no golden files to go stale.
#
# This is the gate that matters for stage08 front-end work.  run-tests.sh is
# 60 small programs and stays green through defects the amalgamation dies
# on: the GitHub-issue-39 batch left `#elif !defined(__GNUC__) /* comment */`
# selecting the right branch at last, which changed which SQLITE_INT_TO_PTR
# SQLite uses, and sqlite3.c stopped compiling with the suite still 60/60.
# 265,876 lines of someone else's C is the only thing that finds those.
#
#   ./check-stage08.sh              # both builds, both programs, diff
#   ./check-stage08.sh --no-clang   # reuse an existing out/ from build.sh
#
# ~2 minutes, most of it the two amalgamation compiles.
set -e
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$SCRIPT_DIR/.."
EMU="${SELFHOST_EMU:-$ROOT/tools/dbt/slow32-dbt}"
[ -x "$EMU" ] || EMU="$ROOT/tools/emulator/slow32-fast"
WORK="$(mktemp -d /tmp/sqlite-accept.XXXXXX)"
trap 'rm -rf "$WORK"' EXIT
SQL="$SCRIPT_DIR/tests/acceptance.sql"
FAIL=0

if [ "${1:-}" != "--no-clang" ]; then
    echo "=== clang build (the oracle)"
    "$SCRIPT_DIR/build.sh" > "$WORK/clang.log" 2>&1 || { tail -20 "$WORK/clang.log"; echo "clang build failed" >&2; exit 1; }
fi
[ -f "$SCRIPT_DIR/out/sqlite3_test.s32x" ] || { echo "no out/sqlite3_test.s32x -- run build.sh" >&2; exit 1; }

echo "=== stage08 build"
"$SCRIPT_DIR/build-stage08.sh" > "$WORK/s8.log" 2>&1 || { tail -20 "$WORK/s8.log"; echo "stage08 build failed" >&2; exit 1; }

# Each program writes its own database: the shell's output depends on it.
# stdout goes to <outfile>; the emulator's exit status is returned
# (GitHub issue 61: a pipe used to discard it).
run() {  # run <exe> <dbname> <outfile> [stdin]
    local exe="$1" db="$WORK/$2" out="$3" sql="${4:-}" rc=0
    rm -f "$db"
    set +e
    if [ -n "$sql" ]; then timeout 300 "$EMU" "$exe" "$db" < "$sql" > "$out" 2>&1
    else timeout 300 "$EMU" "$exe" > "$out" 2>&1; fi
    rc=$?
    set -e
    return "$rc"
}
# The stage08 libc has no getenv (selfhost ISSUES-68), so its shell warns
# about ~/.sqliterc where the clang build reads HOME.  Drop that one line;
# everything else must match byte for byte.
strip() { grep -v "cannot find home directory" || true; }

echo "=== smoke test"
rc_clang=0; rc_s8=0
run "$SCRIPT_DIR/out/sqlite3_test.s32x"         clang.db "$WORK/smoke.clang.raw" || rc_clang=$?
run "$SCRIPT_DIR/out/stage08/sqlite3_test.s32x" s8.db    "$WORK/smoke.s8.raw"    || rc_s8=$?
strip < "$WORK/smoke.clang.raw" > "$WORK/smoke.clang"
strip < "$WORK/smoke.s8.raw"    > "$WORK/smoke.s8"
if diff -q "$WORK/smoke.clang" "$WORK/smoke.s8" > /dev/null; then
    echo "  smoke:  IDENTICAL ($(wc -l < "$WORK/smoke.clang") lines)"
else
    echo "  smoke:  DIFFERS"; diff "$WORK/smoke.clang" "$WORK/smoke.s8" | head -20; FAIL=1
fi
if [ "$rc_clang" -ne "$rc_s8" ]; then
    echo "  smoke:  EXIT STATUS DIFFERS (clang=$rc_clang stage08=$rc_s8)"; FAIL=1
elif [ "$rc_clang" -ne 0 ]; then
    echo "  smoke:  EXIT STATUS $rc_clang (expected 0)"; FAIL=1
fi

if [ -f "$SCRIPT_DIR/out/stage08/sqlite3.s32x" ]; then
    echo "=== shell over tests/acceptance.sql"
    rc_clang=0; rc_s8=0
    run "$SCRIPT_DIR/out/sqlite3.s32x"         clangsh.db "$WORK/shell.clang.raw" "$SQL" || rc_clang=$?
    run "$SCRIPT_DIR/out/stage08/sqlite3.s32x" s8sh.db    "$WORK/shell.s8.raw"    "$SQL" || rc_s8=$?
    strip < "$WORK/shell.clang.raw" > "$WORK/shell.clang"
    strip < "$WORK/shell.s8.raw"    > "$WORK/shell.s8"
    if diff -q "$WORK/shell.clang" "$WORK/shell.s8" > /dev/null; then
        echo "  shell:  IDENTICAL ($(wc -l < "$WORK/shell.clang") lines)"
    else
        echo "  shell:  DIFFERS"; diff "$WORK/shell.clang" "$WORK/shell.s8" | head -30; FAIL=1
    fi
    if [ "$rc_clang" -ne "$rc_s8" ]; then
        echo "  shell:  EXIT STATUS DIFFERS (clang=$rc_clang stage08=$rc_s8)"; FAIL=1
    fi
    # A shell that dies on statement one still "agrees" on the lines it
    # reached, so require the last statement's output too.
    if ! grep -q "integrity_check" "$WORK/shell.s8" ||
       ! grep -q "ok" "$WORK/shell.s8"; then
        echo "  shell:  INCOMPLETE (last statement, pragma integrity_check, never printed)"; FAIL=1
    fi
fi

[ "$FAIL" -eq 0 ] && echo "=== stage08 SQLite acceptance: PASS" || echo "=== stage08 SQLite acceptance: FAIL" >&2
exit $FAIL
