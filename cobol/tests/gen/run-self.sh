#!/bin/bash
# run-self.sh REV FIRST COUNT [STATEMENTS] -- generated programs through two
# compilers: s32-cobc as of git revision REV, and the one in out/.  Each
# program is built and run under both (slow32-fast, each in a directory of
# its own, for the files it writes), and the outputs must be the same
# bytes.  The check for a change that alters code on purpose
# (docs/plans/frontend-pass.md, step 4): tests/asm-snapshot.sh can only
# say the code differs, and runs only what the corpus has; the compiler
# before the change is the oracle here, on as many programs as asked for.
# No container needed.  GEN as run-gen.sh (arith, edit, cond, string,
# table, flow, checked; default flow); STD=85 or 2002 (checked: 2002).
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
EMU="$ROOT/tools/emulator/slow32-fast"
STD=${STD:-85}; [ "$GEN" = checked ] && STD=${STD_CHECKED:-2002}

mkdir -p "$CDIR/out"
W="$(mktemp -d "$CDIR/out/self.XXXXXX")"
# the compiler as of REV, built from its own sources
mkdir -p "$W/old"
git -C "$ROOT" archive "$REV" cobol/src cobol/libcob common | tar -x -C "$W/old"
${CC:-cc} -std=c99 -O1 -w -o "$W/old/s32-cobc" "$W/old/cobol/src/s32-cobc.c" "$W/old/cobol/src/picture.c" "$W/old/cobol/src/picture_scan.c"

last=$((FIRST + COUNT - 1)); bad=0
for s in $(seq "$FIRST" "$last"); do
    python3 "$HERE/gen-$GEN.py" "$s" "$NSTMT" > "$W/g$s.cbl" 2> /dev/null
    for v in old new; do
        mkdir -p "$W/$v$s"
        if [ $v = old ]; then c="$W/old/s32-cobc"; else c="$CDIR/out/s32-cobc"; fi
        if S32_COBC="$c" "$CDIR/compile.sh" -free -std=$STD "$W/g$s.cbl" -o "$W/$v$s/g.s32x" > "$W/$v$s/cc.log" 2>&1; then
            (cd "$W/$v$s" && "$EMU" g.s32x 2>/dev/null | sed '/^Starting execution at PC/,$d' > out.txt) || true
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
