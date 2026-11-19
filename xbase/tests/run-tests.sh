#!/bin/bash
# The interpreter's test suite: each tests/*.txt typed at the dot prompt,
# the output compared with tests/expected/.
#
# EMU names the engine and XBASE the interpreter, so an installed copy
# runs it too (the slow32:xbase image:
# XBASE=/opt/slow32/xbase/xbase.s32x EMU=slow32-fast).
set -e

BASDIR="$(cd "$(dirname "$0")/.." && pwd)"
EMU="${EMU:-$BASDIR/../tools/emulator/slow32-fast}"
# -q: the engine's own notices off.  The filter below drops them by line,
# but "MMIO enabled" comes at exit, and on Linux it lands after a last
# prompt that has no newline, on the prompt's line.
EMU_ARGS="${EMU_ARGS--q --deny term}"
DBASE="${XBASE:-$BASDIR/xbase.s32x}"

if [ ! -f "$DBASE" ]; then
    echo "Error: xbase.s32x not found. Run build.sh first."
    exit 1
fi

PASS=0
FAIL=0
TOTAL=0

# Filter function: remove emulator status lines
filter_output() {
    grep -v \
        -e "Starting execution" \
        -e "^MMIO enabled" \
        -e "^HALT at" \
        -e "^Program halted" \
        -e "^Exit code:" \
        -e "^Instructions executed" \
        -e "^Simulated cycles" \
        -e "^Cycles:" \
        -e "^Wall time:" \
        -e "^Performance:" \
        -e "instructions/second" \
        -e "^Error: Read out of bounds" \
        -e "^Error: Write out of bounds" \
        | sed "s#: $TODAY\$#: TODAY#" \
        || true
}

# LIST STRUCTURE's "Date of last update" is today for a table just written
# (dBASE's): the expected files say TODAY, and today here is the date the
# interpreter sees in this same environment (a builder in UTC is a day
# ahead of the Mac in the evening)
TODAY=$(date +%m/%d/%y)

# Clean up any leftover files from previous runs
rm -f "$BASDIR"/tests/*.DBF "$BASDIR"/tests/*.DBT "$BASDIR"/tests/*.FRM "$BASDIR"/tests/*.LBL "$BASDIR"/tests/*.NDX \
    "$BASDIR"/tests/testfile.txt "$BASDIR"/tests/PRN.TXT "$BASDIR"/tests/ALT*.TXT "$BASDIR"/tests/RESULT*.TXT

for testfile in "$BASDIR"/tests/*.txt; do
    [ -f "$testfile" ] || continue
    name=$(basename "$testfile" .txt)
    expected="$BASDIR/tests/expected/$name.expected"
    TOTAL=$((TOTAL + 1))

    # Clean files before each test
    rm -f "$BASDIR"/tests/*.DBF "$BASDIR"/tests/*.DBT "$BASDIR"/tests/*.FRM "$BASDIR"/tests/*.LBL "$BASDIR"/tests/*.NDX \
        "$BASDIR"/tests/testfile.txt "$BASDIR"/tests/PRN.TXT "$BASDIR"/tests/RESULT*.TXT

    # Run test from tests/ directory so .DBF files are created there
    actual=$( cd "$BASDIR/tests" && cat "$testfile" | "$EMU" $EMU_ARGS "$DBASE" 2>&1 | filter_output )

    if [ -f "$expected" ]; then
        exp=$(cat "$expected")
        if [ "$actual" = "$exp" ]; then
            echo "  PASS: $name"
            PASS=$((PASS + 1))
        else
            echo "  FAIL: $name"
            echo "    Expected:"
            echo "$exp" | head -5 | sed 's/^/      /'
            echo "    Got:"
            echo "$actual" | head -5 | sed 's/^/      /'
            FAIL=$((FAIL + 1))
        fi
    else
        # No expected file — just check it doesn't error
        if echo "$actual" | grep -qi "^error"; then
            echo "  FAIL: $name (error in output)"
            echo "$actual" | grep -i "error" | head -3 | sed 's/^/      /'
            FAIL=$((FAIL + 1))
        else
            echo "  PASS: $name (no .expected file, no errors)"
            PASS=$((PASS + 1))
        fi
    fi
done

# Timed INKEY needs the term service (wait-for-any on stdin). The rest of
# the suite denies term so @SAY stays line-mode. DO a .PRG so the program
# text is a file; S32_STDIN_PREFIX is only the DO line; keys are real stdin.
run_inkey() {
    local name=$1
    local prg=$2
    local keys=$3
    local want=$4
    TOTAL=$((TOTAL + 1))
    local prefix="$BASDIR/tests/.$name.dot"
    printf 'DO %s\n' "$prg" > "$prefix"
    local actual
    if [ -n "$keys" ]; then
        actual=$( cd "$BASDIR/tests" && env S32_STDIN_PREFIX="$prefix" \
            timeout 5 "$EMU" "$DBASE" < <(sleep 0.3; printf '%s' "$keys") 2>&1 | filter_output )
    else
        actual=$( cd "$BASDIR/tests" && env S32_STDIN_PREFIX="$prefix" \
            timeout 5 "$EMU" "$DBASE" </dev/null 2>&1 | filter_output )
    fi
    rm -f "$prefix"
    if echo "$actual" | grep -q "$want"; then
        echo "  PASS: $name"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $name (want /$want/)"
        echo "$actual" | head -8 | sed 's/^/      /'
        FAIL=$((FAIL + 1))
    fi
}
run_inkey test_inkey_poll INKPOLL.PRG "" $'^ *0$'
run_inkey test_inkey_timeout INKWAIT.PRG "" $'^ *0$'
run_inkey test_inkey_key INKWAIT.PRG "A" $'^ *65$'
run_inkey test_get_overtype GETOVR.PRG $'1\r' $'^ 1$'
run_inkey test_get_overtype_redraw GETOVR.PRG $'1\r' '2;3H.\[2;2H 1'
run_inkey test_typeahead TYPEAHD.PRG "A" $'^ *65$'
run_inkey test_get_utf8 GETU8.PRG $'\xe6\x97\xa5\e[F\xc3\xb6\r' '^\[日aïveö  \] *8 日 é \[\]$'
run_inkey test_screen_utf8 SCRU8.PRG "" '^ *20 \.T\. *229 *101 *7 *32$'
run_inkey test_char_get CHARGET.PRG $'qr\r' $'^Qr\\[   \\]$'
run_inkey test_template_get TMPLGET.PRG $'\r' '2;2H   -    '
run_inkey test_field_get_kept FLDGET.PRG $'\r\r' '^01/24/94\[        \]$'
run_inkey test_field_get_typed FLDGET.PRG '0225955551234' '^02/25/95\[555-1234\]$'

# The printer is PRN, a file: $XBASE_PRN names it (PRN.TXT without it), and
# what only the printer is given never reaches the screen
TOTAL=$((TOTAL + 1))
prn_out=$( cd "$BASDIR/tests" && printf 'SET CONSOLE OFF\nSET PRINTER ON\n? "to the printer"\nSET PRINTER OFF\nSET CONSOLE ON\nEJECT\nQUIT\n' |
    env XBASE_PRN="$BASDIR/tests/RESULT_PRN.TXT" "$EMU" $EMU_ARGS "$DBASE" 2>&1 | filter_output )
if grep -q "to the printer" "$BASDIR/tests/RESULT_PRN.TXT" 2>/dev/null &&
   [ "$(LC_ALL=C tr -cd '\f' < "$BASDIR/tests/RESULT_PRN.TXT" | wc -c | tr -d ' ')" = 1 ] &&
   ! echo "$prn_out" | grep -q "to the printer\|PAGE"; then
    echo "  PASS: test_prn_file"; PASS=$((PASS + 1))
else
    echo "  FAIL: test_prn_file"; echo "$prn_out" | tail -3 | sed 's/^/      /'; FAIL=$((FAIL + 1))
fi

# Index files in this interpreter's format before dBASE III's (NDX2,
# tests/ndx2) are converted when opened: they walk as fresh ones do, and
# are dBASE's afterwards (no NDX2 magic left)
TOTAL=$((TOTAL + 1))
cp "$BASDIR"/tests/ndx2/* "$BASDIR/tests/"
conv_out=$( cd "$BASDIR/tests" && printf 'DO NDX2CONV\nQUIT\n' | "$EMU" $EMU_ARGS "$DBASE" 2>&1 | filter_output )
conv_magic=$(cd "$BASDIR/tests" && cat OLDC.NDX OLDN.NDX OLDD.NDX OLDCD.NDX OLDCF.NDX | LC_ALL=C grep -c '2XDN' || true)
if echo "$conv_out" | grep -q 'converted: .T. .T. .T. .T. .T.' &&
   echo "$conv_out" | grep -q 'FOR kept: .T.' && [ "$conv_magic" = 0 ]; then
    echo "  PASS: test_ndx2_convert"; PASS=$((PASS + 1))
else
    echo "  FAIL: test_ndx2_convert"; echo "$conv_out" | tail -3 | sed 's/^/      /'; FAIL=$((FAIL + 1))
fi

# Index files byte for byte as dBASE III PLUS writes them (tests/dbase3:
# each .PRG run on its .DBF by dBASE itself, the indexes it left with
# the bytes it never sets made 0 -- normalize.py says which): splits,
# equal keys in the order they come, keys and pages that leave, blank
# dates, a compound key; and INDEX ON's tree, one leaf a level up, which
# xbase reads from dBASE's file in key order
TOTAL=$((TOTAL + 1))
d3dir=$(mktemp -d)
cp "$BASDIR"/tests/dbase3/*.DBF "$BASDIR"/tests/dbase3/*.PRG "$d3dir/"
cp "$BASDIR/tests/dbase3/BULKC.NDX" "$d3dir/BULKG.NDX"
d3_out=$( cd "$d3dir" && printf 'DO NDXK\nDO BULK\nDO BULKW\nQUIT\n' | "$EMU" $EMU_ARGS "$DBASE" 2>&1 | filter_output )
d3_bad=""
for f in NDXKC NDXKN NDXKD NDXKX BULKC; do
    cmp -s "$d3dir/$f.NDX" "$BASDIR/tests/dbase3/$f.NDX" || d3_bad="$d3_bad $f"
done
if [ -z "$d3_bad" ] && echo "$d3_out" | grep -q "dBASE's walk: *993 \.T\. *99984"; then
    echo "  PASS: test_dbase3_ndx"; PASS=$((PASS + 1))
else
    echo "  FAIL: test_dbase3_ndx${d3_bad:+ (differ:$d3_bad)}"; echo "$d3_out" | tail -3 | sed 's/^/      /'; FAIL=$((FAIL + 1))
fi
rm -rf "$d3dir"

# The interpreter's display as dBASE III PLUS's own (tests/dbase3/WIDTHS.TXT
# is what dBASE III PLUS 1.1 wrote running WIDTHS.PRG): number widths,
# SET DECIMALS, parameters, a zero divisor, LIST and DISPLAY, the TALK lines
TOTAL=$((TOTAL + 1))
d3dir=$(mktemp -d)
cp "$BASDIR"/tests/dbase3/WIDTHS.DBF "$BASDIR"/tests/dbase3/WIDTHS.PRG "$BASDIR"/tests/dbase3/WPARM.PRG "$d3dir/"
( cd "$d3dir" && printf 'DO WIDTHS\nQUIT\n' | "$EMU" $EMU_ARGS "$DBASE" >/dev/null 2>&1 )
d3out=$(ls "$d3dir" | grep -i '^wout\.txt$' | head -1)
if [ -n "$d3out" ] && cmp -s <(tr -d '\r' < "$d3dir/$d3out") "$BASDIR/tests/dbase3/WIDTHS.TXT"; then
    echo "  PASS: test_dbase3_display"; PASS=$((PASS + 1))
else
    echo "  FAIL: test_dbase3_display"
    [ -n "$d3out" ] && diff <(tr -d '\r' < "$d3dir/$d3out") "$BASDIR/tests/dbase3/WIDTHS.TXT" | head -5 | sed 's/^/      /'
    FAIL=$((FAIL + 1))
fi
rm -rf "$d3dir"

# Final cleanup
rm -f "$BASDIR"/tests/*.DBF "$BASDIR"/tests/*.DBT "$BASDIR"/tests/*.FRM "$BASDIR"/tests/*.LBL "$BASDIR"/tests/*.NDX \
    "$BASDIR"/tests/testfile.txt "$BASDIR"/tests/PRN.TXT "$BASDIR"/tests/ALT*.TXT "$BASDIR"/tests/RESULT*.TXT

echo ""
echo "$PASS/$TOTAL passed, $FAIL failed"
exit $FAIL
