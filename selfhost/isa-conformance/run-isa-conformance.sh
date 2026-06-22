#!/usr/bin/env bash
# run-isa-conformance.sh -- Differential ISA-conformance oracle for SLOW-32.
#
# Generates a directed, self-checking instruction corpus (gen_corpus.py) and
# runs every test program on every available emulator. Each test bakes in the
# EXPECTED result of every operation, computed by an independent reference
# model (gen_corpus.py), as `assert_eq` checks. A divergent engine halts on a
# failed assert and never prints its `CHK=XXXXXXXX` marker.
#
# Pass criteria per (engine, test):
#   - stdout contains a `CHK=` marker        -> all asserts passed
#   - no "Assert" line on stdout/stderr
#   - the CHK value matches every other engine AND the reference (expected.txt)
#
# A failure on ONE engine = that engine diverges from the reference.
# A failure on ALL engines for one case = the reference and every engine
# disagree -- the correlated-error residual the artifact-equality check cannot
# surface. Both are reported.
#
# Usage: run-isa-conformance.sh [build_dir]
# Exit:  0 all pass, 1 a divergence was found, 2 setup error.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$SCRIPT_DIR"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

BUILD="${1:-/tmp/isa-conformance}"
EXEC_TIMEOUT="${EXEC_TIMEOUT:-30}"

ASM="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"

mkdir -p "$BUILD"
fail() { echo "ERR: $*" >&2; exit 2; }
[[ -x "$ASM" ]] || fail "assembler not built: $ASM (run: make)"
[[ -x "$LD"  ]] || fail "linker not built: $LD (run: make)"

# --- Build the stage00 C trust-root emulator from source (the symlink in
#     selfhost/stage00 points at the DBT, so build the real C binary here). ---
S32EMU_CC="$BUILD/s32-emu-cc"
if ! cc -O2 -o "$S32EMU_CC" "$ROOT/selfhost/stage00/s32-emu.c" 2>"$BUILD/s32emu-build.log"; then
    cat "$BUILD/s32emu-build.log" >&2
    fail "failed to build stage00 C emulator"
fi

# --- Assemble the emulator roster (label -> command). Skip those not present. ---
declare -a LABELS=()
declare -a CMDS=()
add_emu() {  # add_emu <label> <command...>
    local label="$1"; shift
    local bin="$1"
    if [[ "$bin" == /* || "$bin" == */* ]]; then
        [[ -x "$bin" ]] || { echo "skip $label (missing: $bin)" >&2; return; }
    else
        command -v "$bin" >/dev/null 2>&1 || { echo "skip $label (not on PATH: $bin)" >&2; return; }
    fi
    LABELS+=("$label"); CMDS+=("$*")
}
add_emu "stage00-cc"  "$S32EMU_CC"
add_emu "slow32"      "$ROOT/tools/emulator/slow32"
add_emu "slow32-fast" "$ROOT/tools/emulator/slow32-fast"
add_emu "slow32-dbt"  "$ROOT/tools/dbt/slow32-dbt"

# QEMU: prefer a local qemu-system-slow32, else run it inside the
# slow32:emulator container (build dir mounted at /data). See QEMU_IMG below.
QEMU_IMG="${QEMU_IMG:-slow32:emulator}"
if command -v qemu-system-slow32 >/dev/null 2>&1; then
    LABELS+=("qemu"); CMDS+=("LOCAL_QEMU")
elif command -v docker >/dev/null 2>&1 && docker image inspect "$QEMU_IMG" >/dev/null 2>&1; then
    LABELS+=("qemu"); CMDS+=("DOCKER_QEMU")
    echo "qemu via container image: $QEMU_IMG" >&2
else
    echo "skip qemu (no local qemu-system-slow32, no $QEMU_IMG image)" >&2
fi

[[ ${#LABELS[@]} -gt 0 ]] || fail "no emulators available"
echo "engines: ${LABELS[*]}"

# Dispatch one (engine, test) -> combined stdout/stderr. stdin from /dev/null so
# QEMU's stdio monitor never blocks; timeout bounds a hung/looping engine.
run_engine() {  # $1 = command token, $2 = test base name
    local cmd="$1" t="$2"
    case "$cmd" in
    DOCKER_QEMU)
        timeout "$EXEC_TIMEOUT" docker run --rm -v "$BUILD:/data" "$QEMU_IMG" \
            s32run --qemu "/data/$t.s32x" </dev/null 2>&1 ;;
    LOCAL_QEMU)
        timeout "$EXEC_TIMEOUT" qemu-system-slow32 -machine slow32-tcg \
            -kernel "$BUILD/$t.s32x" -nographic </dev/null 2>&1 ;;
    *)
        timeout "$EXEC_TIMEOUT" $cmd "$BUILD/$t.s32x" </dev/null 2>&1 ;;
    esac
}

# --- Generate + build the corpus. ---
python3 "$SCRIPT_DIR/gen_corpus.py" "$BUILD" || fail "corpus generation failed"

declare -A EXPECT
while read -r name chk; do EXPECT["$name"]="$chk"; done < "$BUILD/expected.txt"

shopt -s nullglob
tests=()
for s in "$BUILD"/test_*.s; do
    base="$(basename "${s%.s}")"
    if ! "$ASM" "$s" "$BUILD/$base.s32o" >"$BUILD/$base.asm.log" 2>&1; then
        grep -i error "$BUILD/$base.asm.log" >&2
        fail "assembly failed: $base"
    fi
    "$LD" -o "$BUILD/$base.s32x" "$BUILD/$base.s32o" >>"$BUILD/$base.asm.log" 2>&1 \
        || fail "link failed: $base"
    tests+=("$base")
done
[[ ${#tests[@]} -gt 0 ]] || fail "no tests generated"

# --- Run the matrix. ---
fails=0
: >"$BUILD/failures.log"   # truncate: stale entries must not survive across runs
printf '\n%-16s' "TEST"
for L in "${LABELS[@]}"; do printf '%-13s' "$L"; done
printf '%-10s\n' "REF"

for t in "${tests[@]}"; do
    printf '%-16s' "$t"
    ref="${EXPECT[$t]}"
    for i in "${!LABELS[@]}"; do
        out="$(run_engine "${CMDS[$i]}" "$t")"
        chk="$(grep -aoE 'CHK=[0-9A-F]{8}' <<<"$out" | head -1 | cut -d= -f2)"
        if [[ -z "$chk" ]]; then
            pc="$(grep -oE 'PC[=: ]*0x[0-9A-Fa-f]+' <<<"$out" | head -1)"
            printf '%-13s' "FAIL"
            echo "  [$t/${LABELS[$i]}] no marker (assert/halt) $pc" >>"$BUILD/failures.log"
            fails=$((fails+1))
        elif [[ "$chk" != "$ref" ]]; then
            printf '%-13s' "DIFF:$chk"
            echo "  [$t/${LABELS[$i]}] checksum $chk != ref $ref" >>"$BUILD/failures.log"
            fails=$((fails+1))
        else
            printf '%-13s' "ok"
        fi
    done
    printf '%-10s\n' "$ref"
done

echo
if [[ $fails -eq 0 ]]; then
    echo "PASS: ${#tests[@]} tests x ${#LABELS[@]} engines, all agree with reference"
    exit 0
fi
echo "FAIL: $fails divergence(s) -- see $BUILD/failures.log"
sed 's/^/  /' "$BUILD/failures.log" >&2
exit 1
