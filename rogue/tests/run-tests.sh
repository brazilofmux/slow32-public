#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROGUE_DIR="$(dirname "$SCRIPT_DIR")"
ROOT="$(dirname "$ROGUE_DIR")"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"

echo "Building rogue..."
(cd "$ROGUE_DIR" && bash build.sh >/dev/null)

if [ ! -x "$EMU" ]; then
    echo "emulator not found: $EMU"
    exit 1
fi

fail=0

check() {
    local name="$1" out="$2"
    shift 2
    local pat bad=0
    for pat in "$@"; do
        if grep -q "$pat" "$out"; then
            echo "  OK  $name: $pat"
        else
            echo "  FAIL $name: $pat"
            bad=1
        fi
    done
    if [ "$bad" -ne 0 ]; then
        echo "=== $name output (tail) ---"
        tail -40 "$out"
        fail=1
    fi
}

work="$(mktemp -d "${TMPDIR:-/tmp}/s32-rogue.XXXXXX")"
trap 'rm -rf "$work"' EXIT

run() { # run <workdir> <seed> <extra flags...> ; input on stdin, output on stdout
    local dir="$1" seed="$2"
    shift 2
    (cd "$dir" && "$EMU" "$ROGUE_DIR/rogue.s32x" --seed "$seed" --line "$@")
}

echo "=== Rogue tests ==="

# 1. Startup: welcome, the player on the map, the status line.
mkdir -p "$work/t1"
printf 'i\n' | run "$work/t1" 7 > "$work/t1.out" 2>&1 || true
check startup "$work/t1.out" \
    "Welcome to the Dungeons of Doom" "@" "Level: 1 " \
    "a) some food" "mace (weapon in hand)" "ring mail (being worn)"

# 2. Wizard descent: stairs teleport + '>' reaches level 2, then level 3.
mkdir -p "$work/t2"
printf '&\n>\n&\n>\n' | run "$work/t2" 7 --wizard > "$work/t2.out" 2>&1 || true
check descend "$work/t2.out" \
    "You descend to level 2" "You descend to level 3" "Level: 3 "

# 3. Eating: consumes the ration.
mkdir -p "$work/t3"
printf 'e\ni\ne\n' | run "$work/t3" 7 > "$work/t3.out" 2>&1 || true
check eat "$work/t3.out" \
    "Yum, that tasted good" "You have nothing to eat"

# 4. The full crawl: dive to 26, grab the amulet the wizard way, climb out.
mkdir -p "$work/t4"
{
    for i in $(seq 1 25); do printf '+\n&\n>\n'; done
    printf 'A\n'
    for i in $(seq 1 25); do printf '+\n&\n<\n'; done
    printf '+\n&\n<\n'
} | run "$work/t4" 1234 --wizard > "$work/t4.out" 2>&1 || true
check winner "$work/t4.out" \
    "You descend to level 26" "Amulet of Yendor" \
    "You climb up to level 1" "Total winner" \
    "escaped with the Amulet"
if [ -f "$work/t4/rogue.scr" ]; then
    echo "  OK  winner: rogue.scr written"
else
    echo "  FAIL winner: rogue.scr missing"
    fail=1
fi

# 5. Save and restore: 'S' writes rogue.sav, next run consumes it.
mkdir -p "$work/t5"
printf '&\n>\nS\n' | run "$work/t5" 7 --wizard > "$work/t5a.out" 2>&1 || true
check save "$work/t5a.out" "Game saved"
if [ ! -f "$work/t5/rogue.sav" ]; then
    echo "  FAIL save: rogue.sav missing"
    fail=1
else
    echo "  OK  save: rogue.sav written"
fi
printf 'i\n' | (cd "$work/t5" && "$EMU" "$ROGUE_DIR/rogue.s32x" --line) \
    > "$work/t5b.out" 2>&1 || true
check restore "$work/t5b.out" "Welcome back" "Level: 2 "
if [ -f "$work/t5/rogue.sav" ]; then
    echo "  FAIL restore: rogue.sav not consumed (permadeath!)"
    fail=1
else
    echo "  OK  restore: rogue.sav consumed"
fi

# 6. Level generation soak: many seeds, deep dive, no faults.
mkdir -p "$work/t6"
soak_fail=0
for seed in 1 2 3 4 5 6 7 8 9 10; do
    { for i in $(seq 1 25); do printf '+\n&\n>\n'; done; printf 'Q\n'; } \
        | run "$work/t6" "$seed" --wizard > "$work/t6.out" 2>&1 || true
    if ! grep -q "You descend to level 26" "$work/t6.out"; then
        echo "  FAIL soak: seed $seed did not reach level 26"
        tail -5 "$work/t6.out"
        soak_fail=1
    fi
done
if [ "$soak_fail" -eq 0 ]; then
    echo "  OK  soak: 10 seeds x 25 levels generated clean"
else
    fail=1
fi

if [ "$fail" -ne 0 ]; then
    echo "=== TESTS FAILED ==="
    exit 1
fi
echo "=== All tests passed ==="
