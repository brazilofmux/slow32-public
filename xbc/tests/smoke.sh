#!/bin/bash
# The slow32:xbase image's smoke test: compile a small program with the
# installed driver, run it, and run the same program in the interpreter;
# both must print what is written below.  The program passes a number to
# a code block and divides, the two shapes that once went wrong between
# stage08 cc and the clang-built runtime (selfhost ISSUES-102).
#
#   XBC_COMPILE  the compile command, prog.prg out.s32x (default s32xbc)
#   XBASE_CMD    the interpreter command (default xbase)
#   EMU          the engine for the compiled program (default slow32-fast)
#
# In the tree: XBC_COMPILE="bash xbc/compile-prg.sh"
#   XBASE_CMD="tools/emulator/slow32-fast -q xbase/xbase.s32x"
#   EMU=tools/emulator/slow32-fast  (paths absolute: it runs in a temp dir)
set -u
XBC_COMPILE="${XBC_COMPILE:-s32xbc}"
XBASE_CMD="${XBASE_CMD:-xbase}"
EMU="${EMU:-slow32-fast}"
W=$(mktemp -d "${TMPDIR:-/tmp}/xbcsmoke.XXXXXX") || exit 2
trap 'rm -rf "$W"' EXIT
cd "$W" || exit 2

cat > SMOKE.PRG <<'PRG'
* smoke test: a greeting, a code block, a division
LOCAL bTwice := {|n| n * 2}
? "Hello from SLOW-32 xBase"
? EVAL(bTwice, 21)
? STR(7 / 2, 6, 2)
RETURN
PRG
printf '\nHello from SLOW-32 xBase\n        42\n  3.50' > want

# the image carries no python: bytes trimmed with the shell's tools
rstrip_nl() {   # FILE: its bytes without trailing newlines
    local n; n=$(($(wc -c < "$1")))
    while [ "$n" -gt 0 ] &&
          [ "$(tail -c +"$n" "$1" | head -c 1 | od -An -c | tr -d ' ')" = '\n' ]; do
        n=$((n - 1))
    done
    head -c "$n" "$1"
}

fail=0
if ! $XBC_COMPILE SMOKE.PRG "$W/smoke.s32x" > cc.log 2>&1; then
    echo "smoke: compile FAILED"; cat cc.log; exit 1
fi
"$EMU" -q smoke.s32x < /dev/null > got.c 2>&1
rstrip_nl got.c > got.cn
if cmp -s want got.cn; then echo "smoke: compiled  OK"; else
    echo "smoke: compiled  DIFFER"; od -c got.c | head -20; fail=1
fi

printf 'SET TALK OFF\nDO SMOKE\nQUIT\n' | $XBASE_CMD > got.i 2>&1
# the banner (3 lines), the prompts of SET TALK OFF and DO, the QUIT prompt
tail -n +4 got.i > t1
if [ "$(head -c 4 t1)" = ". . " ]; then tail -c +5 t1 > t2; else cat t1 > t2; fi
n=$(($(wc -c < t2)))
if [ "$n" -ge 2 ] && [ "$(tail -c 2 t2)" = ". " ]; then head -c $((n - 2)) t2 > t1; else cat t2 > t1; fi
rstrip_nl t1 > got.in
if cmp -s want got.in; then echo "smoke: interpreter OK"; else
    echo "smoke: interpreter DIFFER"; od -c got.i | head -20; fail=1
fi
exit $fail
