#!/bin/bash
# run-flag.sh FLAG FIRST COUNT [N] -- generated programs through one
# compiler twice: as it is, and with FLAG (a compiler option that turns a
# rewrite off: -fno-loop-reg, -fno-hot-arith, -fno-native-items).  Each program is built and
# run both ways (slow32-fast, each in a directory of its own, for the
# files it writes) and must print the same bytes.  The check for a
# rewrite that is meant to change nothing a program can see: the code
# without it is the oracle, on as many programs as asked for, and the two
# sides differ in the rewrite alone.  GEN as run-gen.sh (default loop);
# STD=85 or 2002 (loop, checked, pos, perf, lit: 2002).
# Keeps the work directory when anything differs.  A program that does
# not build is a failure, not an agreement.
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
CDIR="$(cd "$HERE/../.." && pwd)"
ROOT="$(cd "$CDIR/.." && pwd)"
FLAG=${1:?usage: run-flag.sh FLAG FIRST COUNT [N]}
FIRST=${2:?usage: run-flag.sh FLAG FIRST COUNT [N]}
COUNT=${3:?usage: run-flag.sh FLAG FIRST COUNT [N]}
GEN=${GEN:-loop}
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"     # the image has it at /usr/local/bin and no tree build
STD=${STD:-85}; case "$GEN" in loop|checked|pos|perf|lit|native) STD=2002 ;; esac

command -v python3 >/dev/null 2>&1 || { echo "run-flag.sh: no python3 to write the programs with"; exit 2; }
mkdir -p "$CDIR/out"
W="$(mktemp -d "$CDIR/out/flag.XXXXXX")"
last=$((FIRST + COUNT - 1)); bad=0; lines=0
for s in $(seq "$FIRST" "$last"); do
    if ! python3 "$HERE/gen-$GEN.py" "$s" ${4:+"$4"} > "$W/g$s.cbl" 2> "$W/g$s.genlog"; then
        echo "seed $s: GENERATOR FAILED ($(head -c 100 "$W/g$s.genlog" | tr '\n' ' '))"; bad=$((bad + 1)); continue
    fi
    for v in with without; do
        mkdir -p "$W/$v$s"
        f=""; [ $v = with ] || f="$FLAG"
        if "$CDIR/compile.sh" -free -std=$STD $f "$W/g$s.cbl" -o "$W/$v$s/g.s32x" > "$W/$v$s/cc.log" 2>&1; then
            # capped: a program that never ends differs, it does not hang the batch
            # -q: the program's output alone.  Cutting the emulator's banner
            # off with sed depended on where stdout buffering put it: on
            # Linux it came first and took every line with it, and the two
            # empty outputs agreed (builder-de, 2026-10-02).
            (cd "$W/$v$s" && "$EMU" -q -c 2000000000 g.s32x > out.txt 2>/dev/null) || true
        else
            echo BUILD-FAILED > "$W/$v$s/out.txt"
        fi
    done
    if grep -q BUILD-FAILED "$W/with$s/out.txt" "$W/without$s/out.txt"; then
        echo "seed $s: BUILD FAILED ($(grep -h -m1 error "$W/with$s/cc.log" "$W/without$s/cc.log" | head -1 | cut -c1-100))"; bad=$((bad + 1))
    elif cmp -s "$W/with$s/out.txt" "$W/without$s/out.txt"; then
        lines=$((lines + $(wc -l < "$W/with$s/out.txt")))
        rm -rf "$W/with$s" "$W/without$s" "$W/g$s.cbl"
    else
        echo "seed $s: DIFFERS"; bad=$((bad + 1))
    fi
done
# an agreement over nothing is not one: the programs print, so a run
# that compared no lines did not run them
if [ $bad = 0 ] && [ $lines = 0 ]; then echo "all $COUNT the same but NOTHING COMPARED (0 lines); kept $W"; exit 1; fi
if [ $bad = 0 ]; then rm -rf "$W"; echo "all $COUNT the same ($lines lines)"; else echo "$bad of $COUNT differ; kept $W"; fi
