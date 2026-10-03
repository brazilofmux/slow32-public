#!/bin/bash
# sanitize.sh -- the compiler under the address and undefined-behavior
# sanitizers, over everything there is to compile: tests/free, fixed and
# 2002, the exception-site templates, a batch of every tests/gen
# generator, and (when it is there) majesty's sources.  A program the compiler refuses is no finding; a
# sanitizer report is, and names its program.
#
# Why: `&g_desc[sym_desc(s)]` read the descriptor table's address and
# called a function that may move the table, in an order C does not fix.
# It compiled every test for months and crashed on one statement the day
# a new path made a descriptor at the wrong moment (cobol ISSUES-122).
# Seconds; run-tests.sh runs it as Gate 8.  Exit 2 when the host cannot
# build with the sanitizers (reported, not passed over).
#
#   tests/sanitize.sh            MAJESTY=<dir> for the corpus (default ~/majesty)
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"; C="$HERE/.."
W="$(mktemp -d)"; trap 'rm -rf "$W"' EXIT
if ! ${CC:-cc} -std=c99 -O0 -g -fsanitize=address,undefined -w -o "$W/cobc" "$C/src/s32-cobc.c" "$C/src/picture.c" "$C/src/picture_scan.c" 2> "$W/cc.log"; then
    echo "sanitize: NOT RUN -- this host's compiler builds no sanitizer binary ($(head -1 "$W/cc.log" | cut -c1-80))"; exit 2
fi
n=0; bad=0
one() {   # one flags... file
    n=$((n + 1))
    # (detect_leaks=0: the compiler frees nothing at exit, by design, and
    # LeakSanitizer -- on by default on Linux, off on macOS -- reported every
    # program for it: 721 of 721 on kagura)
    ASAN_OPTIONS="detect_leaks=0${ASAN_OPTIONS:+:$ASAN_OPTIONS}" "$W/cobc" "$@" -o /dev/null > "$W/log" 2>&1 && return
    if grep -q 'AddressSanitizer\|runtime error' "$W/log"; then
        bad=$((bad + 1)); echo "FINDING: ${*: -1}"; grep -m3 'ERROR\|runtime error\|#0 \|#1 ' "$W/log"
    fi
}
for f in "$HERE"/free/*.cbl; do one -free -I "$HERE/copy" "$f"; done
for f in "$HERE"/fixed/*.cbl; do one -fixed -I "$HERE/copy" "$f"; done
for f in "$HERE"/2002/*.cbl; do one -free -std=2002 -I "$HERE/copy" "$f"; done
for g in "$HERE"/gen/gen-*.py; do
    for s in $(seq 1 20); do
        python3 "$g" "$s" > "$W/g.cbl" 2>/dev/null || continue
        one -free -std=2002 "$W/g.cbl"
    done
done
# the exception-site templates, a statement at a time (run-tests.sh gate 6)
for pair in template.cbl:sites.txt argfn.cbl:argfn.txt; do
    t="$HERE/ecsites/${pair%%:*}"; l="$HERE/ecsites/${pair##*:}"
    [ -f "$t" ] && [ -f "$l" ] || continue
    while IFS= read -r line || [ -n "$line" ]; do
        case "$line" in ""|"#"*) continue ;; esac
        awk -v s="${line#*|}" '{ i = index($0, "@STMT@"); if (i) $0 = substr($0, 1, i - 1) s substr($0, i + 6); print }' "$t" > "$W/e.cbl"
        one -free -std=2002 "$W/e.cbl"
    done < "$l"
done
M="${MAJESTY:-$HOME/majesty}"
if [ -d "$M/src/cobol" ]; then
    for f in "$M"/src/cobol/*.cbl; do one -free -I "$M/src/copy" -I "$M/src/h" "$f"; done
fi
if [ $bad = 0 ]; then echo "sanitize: $n programs compiled, no findings"; else echo "sanitize: $bad finding(s) in $n programs"; exit 1; fi
