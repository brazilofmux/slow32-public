#!/bin/bash
# run-self.sh REV FIRST COUNT [STATEMENTS] -- generated programs through two
# compilers: s32-cobc as of git revision REV (with the runtime as of REV),
# and the one in out/ (with the runtime in libcob/).  Each
# program is built and run under both (slow32-fast, each in a directory of
# its own, for the files it writes), and the outputs must be the same
# bytes.  The check for a change that alters code on purpose
# (docs/plans/frontend-pass.md, step 4): tests/asm-snapshot.sh can only
# say the code differs, and runs only what the corpus has; the compiler
# before the change is the oracle here, on as many programs as asked for.
# No container needed.  GEN as run-gen.sh (arith, edit, cond, string,
# table, flow, checked, pos, perf, lit; default flow); STD=85 or 2002 (checked, perf, lit and
# pos: 2002).
# Keeps the work directory when anything differs.  A program neither
# compiler builds is a failure, not an agreement: two refusals compare
# equal and say nothing.
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
CDIR="$(cd "$HERE/../.." && pwd)"
ROOT="$(cd "$CDIR/.." && pwd)"
REV=${1:?usage: run-self.sh REV FIRST COUNT [STATEMENTS]}
FIRST=${2:?usage: run-self.sh REV FIRST COUNT [STATEMENTS]}
COUNT=${3:?usage: run-self.sh REV FIRST COUNT [STATEMENTS]}
NSTMT=${4:-30}
GEN=${GEN:-flow}
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"
STD=${STD:-85}; case "$GEN" in checked|pos|perf|lit|loop|native) STD=2002 ;; esac

mkdir -p "$CDIR/out"
W="$(mktemp -d "$CDIR/out/self.XXXXXX")"
# the compiler as of REV, built from its own sources
mkdir -p "$W/old"
git -C "$ROOT" archive "$REV" cobol/src cobol/libcob common | tar -x -C "$W/old"
${CC:-cc} -std=c99 -O1 -w -o "$W/old/s32-cobc" "$W/old/cobol/src/s32-cobc.c" "$W/old/cobol/src/picture.c" "$W/old/cobol/src/picture_scan.c" $(ls "$W/old/cobol/src/lex_scan.c" 2>/dev/null)   # the token scanner, from 2026-10-06
# ... and the runtime as of REV, built as libcob/build.sh builds it: a
# change in what the compiler asks of the runtime (a routine's arguments)
# is then on both sides, each compiler with its own
(
    . "$ROOT/cobol/cctool.sh"
    OL="$W/old/cobol/libcob"
    tag=$(cksum < "$OL/kern.h" | awk '{printf "%08x", $1}')
    {
        # the entries written out by hand (libcob/build.sh), when that revision has them
        [ -f "$OL/entries.s" ] && cat "$OL/entries.s"
        printf '\t.text\n'
        for f in cob_get_num cob_put_num_x cob_get_edited cob_put_edited; do
            printf '\t.globl %s\n\t.globl __s32hk_%s_%s\n%s:\n__s32hk_%s_%s:\n\tjal r0, %s_impl\n' \
                "$f" "$f" "$tag" "$f" "$f" "$tag" "$f"
        done
    } > "$OL/libcob_hk.s"
    S32_CC_APPEND="$OL/libcob_hk.s" s32_cc_obj "$OL/libcob.s32o" "$OL/libcob.c" -I"$OL"
) > "$W/old/libcob.log" 2>&1 || { echo "run-self.sh: the runtime as of $REV did not build (see $W/old/libcob.log)" >&2; exit 1; }

last=$((FIRST + COUNT - 1)); bad=0
for s in $(seq "$FIRST" "$last"); do
    python3 "$HERE/gen-$GEN.py" "$s" "$NSTMT" > "$W/g$s.cbl" 2> /dev/null
    for v in old new; do
        mkdir -p "$W/$v$s"
        if [ $v = old ]; then c="$W/old/s32-cobc"; l="$W/old/cobol/libcob/libcob.s32o"; else c="$CDIR/out/s32-cobc"; l="$CDIR/libcob/libcob.s32o"; fi
        if S32_COBC="$c" S32_LIBCOB="$l" "$CDIR/compile.sh" -free -std=$STD "$W/g$s.cbl" -o "$W/$v$s/g.s32x" > "$W/$v$s/cc.log" 2>&1; then
            # capped: a program that never ends (a broken runtime can make one) differs, it does not hang the batch
            (cd "$W/$v$s" && "$EMU" -q -c 2000000000 g.s32x > out.txt 2>/dev/null) || true     # -q: the program's output alone
        else
            echo BUILD-FAILED > "$W/$v$s/out.txt"
        fi
    done
    if grep -q BUILD-FAILED "$W/old$s/out.txt" "$W/new$s/out.txt"; then
        echo "seed $s: BUILD FAILED ($(grep -h -m1 error "$W/new$s/cc.log" "$W/old$s/cc.log" | head -1 | cut -c1-100))"; bad=$((bad + 1))
    elif cmp -s "$W/old$s/out.txt" "$W/new$s/out.txt"; then
        echo "seed $s: same ($(wc -l < "$W/new$s/out.txt" | tr -d ' ') lines)"
        rm -rf "$W/old$s" "$W/new$s" "$W/g$s.cbl"
    else
        echo "seed $s: DIFFERS"; bad=$((bad + 1))
    fi
done
if [ $bad = 0 ]; then rm -rf "$W"; echo "all $COUNT the same"; else echo "$bad of $COUNT differ; kept $W"; fi
