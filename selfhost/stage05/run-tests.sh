#!/usr/bin/env bash
set -euo pipefail

# Stage 05: Full toolchain tests
#
# Tests:
#   1. Build s13cc using stage04's s12cc (compiler bootstrap)
#   2. Run compiler tests with s13cc
#   3. Build tools (s32-as, s32-ar, s32-ld) using stage04's s12cc
#   4. End-to-end: s13cc + stage05 tools compile/assemble/link/run a program

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
TESTS_DIR="$SCRIPT_DIR/tests"

EMU="${SELFHOST_EMU:-}"
EMU_EXPLICIT=0
KEEP_ARTIFACTS=0
RUN_FIXED_POINT=0
STRICT_SHAPE=0

choose_default_emu() {
    local dbt="$SELFHOST_DIR/../tools/dbt/slow32-dbt"
    if [[ -x "$dbt" ]]; then
        printf '%s\n' "$dbt"
    else
        printf '%s\n' "$SELFHOST_DIR/stage00/s32-emu"
    fi
}

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--keep-artifacts] [--fixed-point] [--strict-shape]

Stage13 tests: s12cc compiler + toolchain tests (bootstrapped from stage04)
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --emu)
            shift
            [[ $# -gt 0 ]] || { echo "--emu requires a path" >&2; exit 2; }
            EMU="$1"
            EMU_EXPLICIT=1
            ;;
        --keep-artifacts)
            KEEP_ARTIFACTS=1
            ;;
        --fixed-point)
            RUN_FIXED_POINT=1
            ;;
        --strict-shape)
            STRICT_SHAPE=1
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            usage
            exit 2
            ;;
    esac
    shift
done

if [[ "$EMU_EXPLICIT" -eq 0 && -z "$EMU" ]]; then
    EMU="$(choose_default_emu)"
fi

[[ -f "$EMU" ]] || { echo "Missing emulator: $EMU" >&2; exit 1; }

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage05.XXXXXX)"
if [[ "$KEEP_ARTIFACTS" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
fi

# --- Utility functions ---

run_exe() {
    local exe="$1"
    local log="$2"
    shift 2

    local rc=0
    timeout "${EXEC_TIMEOUT:-180}" "$EMU" "$exe" "$@" >"$log" 2>&1 || rc=$?
    if [[ "$rc" -eq 124 ]]; then
        echo "execution timed out: $exe" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
    if grep -Eq "Execute fault|Memory fault|Write out of bounds|Unknown opcode|Unknown instruction|Load fault|Store fault|Execution limit reached" "$log"; then
        echo "execution faulted: $exe" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "execution did not halt cleanly: $exe (rc=$rc)" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
}

run_exe_rc() {
    local exe="$1"
    local log="$2"
    shift 2

    local rc=0
    timeout "${EXEC_TIMEOUT:-180}" "$EMU" "$exe" "$@" >"$log" 2>&1 || rc=$?
    if [[ "$rc" -eq 124 ]]; then
        echo "execution timed out: $exe" >&2
        tail -n 60 "$log" >&2
        return 124
    fi
    if grep -Eq "Execute fault|Memory fault|Write out of bounds|Unknown opcode|Unknown instruction|Load fault|Store fault|Execution limit reached" "$log"; then
        echo "execution faulted: $exe" >&2
        tail -n 60 "$log" >&2
        return 125
    fi
    return "$rc"
}

check_noop_addi_self() {
    local asm="$1"
    local tag="$2"
    local log="$WORKDIR/${tag}-shape.log"

    if grep -nE '^[[:space:]]*addi r([0-9]+), r\1, 0$' "$asm" >"$log"; then
        echo "  $tag: FAIL (redundant addi rX, rX, 0)" >&2
        cat "$log" >&2
        return 1
    fi
    return 0
}

check_missed_immediate_ops() {
    local asm="$1"
    local tag="$2"
    local log="$WORKDIR/${tag}-imm-shape.log"

    awk '
    function is_small_imm(v) { return (v >= -2048 && v <= 2047); }
    function is_shift_imm(v) { return (v >= 0 && v <= 31); }
    function regnum(tok) {
        if (tok ~ /^r[0-9]+$/) return substr(tok, 2) + 0;
        return -1;
    }
    function is_int(tok) { return (tok ~ /^-?[0-9]+$/); }
    BEGIN { have_prev = 0; bad = 0; }
    {
        orig = $0;
        line = $0;
        gsub(",", "", line);
        gsub(/^[[:space:]]+/, "", line);
        n = split(line, t, /[[:space:]]+/);

        if (n == 4 && t[1] == "addi" && regnum(t[2]) >= 0 && t[3] == "r0" && is_int(t[4])) {
            preg = regnum(t[2]);
            pimm = t[4] + 0;
            pline = NR;
            have_prev = 1;
            next;
        }

        if (have_prev && n == 4) {
            op = t[1];
            rd = regnum(t[2]);
            rA = regnum(t[3]);
            rB = regnum(t[4]);
            if (rd >= 0 && rA >= 0 && rB >= 0) {
                if ((op == "and" || op == "or" || op == "xor") &&
                    (rA == preg || rB == preg) && is_small_imm(pimm)) {
                    printf("%d:%s (from addi at %d)\n", NR, orig, pline);
                    bad = 1;
                } else if ((op == "slt" || op == "sltu") &&
                           rB == preg && is_small_imm(pimm)) {
                    printf("%d:%s (from addi at %d)\n", NR, orig, pline);
                    bad = 1;
                } else if ((op == "sll" || op == "srl" || op == "sra") &&
                           rB == preg && is_shift_imm(pimm)) {
                    printf("%d:%s (from addi at %d)\n", NR, orig, pline);
                    bad = 1;
                }
            }
        }
        have_prev = 0;
    }
    END { exit(bad ? 1 : 0); }
    ' "$asm" >"$log" || {
        if [[ "$STRICT_SHAPE" -eq 1 ]]; then
            echo "  $tag: FAIL (missed immediate opcode shape)" >&2
            cat "$log" >&2
            return 1
        fi
        echo "  $tag: WARN (missed immediate opcode opportunities)" >&2
        cat "$log" >&2
        SHAPE_WARN=$((SHAPE_WARN + 1))
        echo "$tag" >> "$SHAPE_WARN_FILE"
    }
    return 0
}

compile_and_link() {
    local name="$1"
    local src="$2"
    local cc="$3"
    local as="$4"
    local ld="$5"
    local exe="$WORKDIR/${name}.s32x"
    local asm="$WORKDIR/${name}.s"
    local obj="$WORKDIR/${name}.s32o"

    run_exe "$cc" "$WORKDIR/${name}-compile.log" "$src" "$asm"
    if [[ ! -s "$asm" ]]; then
        echo "  $name: FAIL (compile)" >&2
        tail -n 20 "$WORKDIR/${name}-compile.log" >&2
        return 1
    fi
    if [[ "$cc" == "${S13CC_EXE:-}" ]]; then
        check_noop_addi_self "$asm" "$name" || return 1
        check_missed_immediate_ops "$asm" "$name" || return 1
    fi

    run_exe "$as" "$WORKDIR/${name}-assemble.log" "$asm" "$obj"
    if [[ ! -s "$obj" ]]; then
        echo "  $name: FAIL (assemble)" >&2
        tail -n 20 "$WORKDIR/${name}-assemble.log" >&2
        return 1
    fi

    run_exe "$ld" "$WORKDIR/${name}-link.log" \
        -o "$exe" --mmio 64K \
        "$RUNTIME_CRT0" "$obj" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
        $LIBC_OBJS
    if [[ ! -s "$exe" ]]; then
        echo "  $name: FAIL (link)" >&2
        tail -n 20 "$WORKDIR/${name}-link.log" >&2
        return 1
    fi
    printf '%s' "$exe"
}

# ============================================================
# Step 1: Bootstrap — build runtime/libc using stage04 s12cc
# ============================================================
echo "=== Step 1: Bootstrap ==="

STAGE4_CC="$SELFHOST_DIR/stage04/s12cc.s32x"
AS_EXE="$SELFHOST_DIR/stage04/s32-as.s32x"
LD_EXE="$SELFHOST_DIR/stage04/s32-ld.s32x"

[[ -f "$STAGE4_CC" ]] || { echo "Missing s12cc (stage04): $STAGE4_CC" >&2; exit 1; }
[[ -f "$AS_EXE" ]] || { echo "Missing assembler: $AS_EXE" >&2; exit 1; }
[[ -f "$LD_EXE" ]] || { echo "Missing linker: $LD_EXE" >&2; exit 1; }

LIBC_DIR="$SCRIPT_DIR/libc"
CRT0_SRC="$SCRIPT_DIR/crt0.s"
MMIO_NO_START_SRC="$SCRIPT_DIR/mmio_no_start.s"

run_exe "$AS_EXE" "$WORKDIR/crt0.log" "$CRT0_SRC" "$WORKDIR/crt0.s32o"
[[ -s "$WORKDIR/crt0.s32o" ]] || { echo "failed to assemble crt0" >&2; exit 1; }

run_exe "$AS_EXE" "$WORKDIR/mmio_no_start.log" "$MMIO_NO_START_SRC" "$WORKDIR/mmio_no_start.s32o"
[[ -s "$WORKDIR/mmio_no_start.s32o" ]] || { echo "failed to assemble mmio_no_start" >&2; exit 1; }

# Build libc with stage04's s12cc
LIBC_OBJS=""
for name in string_extra string_more ctype convert stdio malloc; do
    run_exe "$STAGE4_CC" "$WORKDIR/${name}.cc.log" "$LIBC_DIR/${name}.c" "$WORKDIR/${name}.s"
    [[ -s "$WORKDIR/${name}.s" ]] || { echo "failed to compile ${name}.c" >&2; exit 1; }
    run_exe "$AS_EXE" "$WORKDIR/${name}.as.log" "$WORKDIR/${name}.s" "$WORKDIR/${name}.s32o"
    [[ -s "$WORKDIR/${name}.s32o" ]] || { echo "failed to assemble ${name}.s" >&2; exit 1; }
    LIBC_OBJS="$LIBC_OBJS $WORKDIR/${name}.s32o"
done

run_exe "$STAGE4_CC" "$WORKDIR/start.cc.log" "$LIBC_DIR/start.c" "$WORKDIR/start.s"
[[ -s "$WORKDIR/start.s" ]] || { echo "failed to compile start.c" >&2; exit 1; }
run_exe "$AS_EXE" "$WORKDIR/start.as.log" "$WORKDIR/start.s" "$WORKDIR/start.s32o"
[[ -s "$WORKDIR/start.s32o" ]] || { echo "failed to assemble start.s" >&2; exit 1; }

RUNTIME_CRT0="$WORKDIR/crt0.s32o"
RUNTIME_MMIO_NO_START_OBJ="$WORKDIR/mmio_no_start.s32o"
LIBC_START_OBJ="$WORKDIR/start.s32o"
STAGE4_LIBC_OBJS="$LIBC_OBJS"
STAGE4_LIBC_START_OBJ="$LIBC_START_OBJ"

echo "Compiler (stage04 s12cc): $STAGE4_CC"
echo "Assembler: $AS_EXE"
echo "Linker: $LD_EXE"

PASS=0
FAIL=0
TOTAL=0
SHAPE_WARN=0
SHAPE_WARN_FILE="$WORKDIR/shape.warn"
: > "$SHAPE_WARN_FILE"

# ============================================================
# Step 2: Build s13cc using stage04's s12cc
# ============================================================
echo ""
echo "=== Step 2: Build s13cc (compiled by stage04 s12cc) ==="

S13CC_SRC="$SCRIPT_DIR/s12cc.c"
[[ -s "$S13CC_SRC" ]] || { echo "ERROR: s12cc.c not found" >&2; exit 1; }

TOTAL=$((TOTAL + 1))
SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300

# Compile stage05 source with stage04 compiler
run_exe "$STAGE4_CC" "$WORKDIR/s13cc-compile.log" "$S13CC_SRC" "$WORKDIR/s13cc.s"
if [[ ! -s "$WORKDIR/s13cc.s" ]]; then
    printf "  %-30s FAIL (compile)\n" "s13cc-build:"
    tail -n 20 "$WORKDIR/s13cc-compile.log" >&2
    FAIL=$((FAIL + 1))
else
    run_exe "$AS_EXE" "$WORKDIR/s13cc-assemble.log" "$WORKDIR/s13cc.s" "$WORKDIR/s13cc.s32o"
    if [[ ! -s "$WORKDIR/s13cc.s32o" ]]; then
        printf "  %-30s FAIL (assemble)\n" "s13cc-build:"
        FAIL=$((FAIL + 1))
    else
        run_exe "$LD_EXE" "$WORKDIR/s13cc-link.log" \
            -o "$WORKDIR/s13cc.s32x" --mmio 64K \
            "$RUNTIME_CRT0" "$WORKDIR/s13cc.s32o" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
            $LIBC_OBJS
        if [[ ! -s "$WORKDIR/s13cc.s32x" ]]; then
            printf "  %-30s FAIL (link)\n" "s13cc-build:"
            FAIL=$((FAIL + 1))
        else
            printf "  %-30s PASS\n" "s13cc-build:"
            PASS=$((PASS + 1))
        fi
    fi
fi
EXEC_TIMEOUT="$SAVED_TIMEOUT"

# ============================================================
# Step 2b: Recompile libc with s13cc (HIR/SSA ABI)
#
# Stage04's tree-walk codegen clobbers r11-r18 without saving them.
# Stage05's register allocator assumes r11-r28 are callee-saved.
# Programs compiled by s13cc must link against s13cc-compiled libc
# to get a consistent ABI.
# ============================================================
S13CC_EXE="$WORKDIR/s13cc.s32x"
G1_LIBC_OBJS=""
G1_LIBC_START_OBJ=""

if [[ -s "$S13CC_EXE" ]]; then
    echo ""
    echo "=== Step 2b: Recompile libc with s13cc (HIR/SSA ABI) ==="
    EXEC_TIMEOUT=300

    for name in string_extra string_more ctype convert stdio malloc; do
        run_exe "$S13CC_EXE" "$WORKDIR/g1_${name}.cc.log" "$LIBC_DIR/${name}.c" "$WORKDIR/g1_${name}.s"
        if [[ ! -s "$WORKDIR/g1_${name}.s" ]]; then
            echo "  WARN: gen1 failed to compile ${name}.c, falling back to stage04 libc" >&2
            G1_LIBC_OBJS=""
            break
        fi
        run_exe "$AS_EXE" "$WORKDIR/g1_${name}.as.log" "$WORKDIR/g1_${name}.s" "$WORKDIR/g1_${name}.s32o"
        if [[ ! -s "$WORKDIR/g1_${name}.s32o" ]]; then
            echo "  WARN: gen1 libc assemble failed: ${name}.s" >&2
            G1_LIBC_OBJS=""
            break
        fi
        G1_LIBC_OBJS="$G1_LIBC_OBJS $WORKDIR/g1_${name}.s32o"
    done

    if [[ -n "$G1_LIBC_OBJS" ]]; then
        run_exe "$S13CC_EXE" "$WORKDIR/g1_start.cc.log" "$LIBC_DIR/start.c" "$WORKDIR/g1_start.s"
        if [[ -s "$WORKDIR/g1_start.s" ]]; then
            run_exe "$AS_EXE" "$WORKDIR/g1_start.as.log" "$WORKDIR/g1_start.s" "$WORKDIR/g1_start.s32o"
            if [[ -s "$WORKDIR/g1_start.s32o" ]]; then
                G1_LIBC_START_OBJ="$WORKDIR/g1_start.s32o"
                echo "  gen1-compiled libc ready (HIR/SSA ABI, r11-r28 callee-saved)"
            fi
        fi
    fi

    EXEC_TIMEOUT="$SAVED_TIMEOUT"
fi

# If gen1 libc was built, use it for all subsequent test linking.
# Runtime asm objects (crt0, mmio_no_start) are ABI-neutral.
if [[ -n "$G1_LIBC_OBJS" && -n "$G1_LIBC_START_OBJ" ]]; then
    LIBC_OBJS="$G1_LIBC_OBJS"
    LIBC_START_OBJ="$G1_LIBC_START_OBJ"
fi

# ============================================================
# Step 3: Test s13cc by compiling and running test programs
# ============================================================

if [[ -s "$S13CC_EXE" ]]; then
    echo ""
    echo "=== Step 3: s13cc compiler tests ==="

    for tst in "$TESTS_DIR"/test_spike.c "$TESTS_DIR"/test_phase2.c "$TESTS_DIR"/test_phase3.c "$TESTS_DIR"/test_phase4.c "$TESTS_DIR"/test_phase5.c "$TESTS_DIR"/test_phase6.c "$TESTS_DIR"/test_phase7.c "$TESTS_DIR"/test_phase8.c "$TESTS_DIR"/test_phase9.c "$TESTS_DIR"/test_phase10.c "$TESTS_DIR"/test_phase11.c "$TESTS_DIR"/test_phase12.c "$TESTS_DIR"/test_phase13.c "$TESTS_DIR"/test_phase14.c; do
        [[ -f "$tst" ]] || continue
        tname="$(basename "$tst" .c)"
        TOTAL=$((TOTAL + 1))

        # Compile with s13cc
        run_exe "$S13CC_EXE" "$WORKDIR/${tname}-s13cc.log" "$tst" "$WORKDIR/${tname}.s"
        if [[ ! -s "$WORKDIR/${tname}.s" ]]; then
            printf "  %-30s FAIL (compile)\n" "$tname:"
            tail -n 20 "$WORKDIR/${tname}-s13cc.log" >&2
            FAIL=$((FAIL + 1))
            continue
        fi
        if ! check_noop_addi_self "$WORKDIR/${tname}.s" "$tname"; then
            printf "  %-30s FAIL (asm shape)\n" "$tname:"
            FAIL=$((FAIL + 1))
            continue
        fi
        if ! check_missed_immediate_ops "$WORKDIR/${tname}.s" "$tname"; then
            printf "  %-30s FAIL (imm shape)\n" "$tname:"
            FAIL=$((FAIL + 1))
            continue
        fi

        # Assemble
        run_exe "$AS_EXE" "$WORKDIR/${tname}-as.log" "$WORKDIR/${tname}.s" "$WORKDIR/${tname}.s32o"
        if [[ ! -s "$WORKDIR/${tname}.s32o" ]]; then
            printf "  %-30s FAIL (assemble)\n" "$tname:"
            tail -n 20 "$WORKDIR/${tname}-as.log" >&2
            FAIL=$((FAIL + 1))
            continue
        fi

        # Link
        run_exe "$LD_EXE" "$WORKDIR/${tname}-ld.log" \
            -o "$WORKDIR/${tname}.s32x" --mmio 64K \
            "$RUNTIME_CRT0" "$WORKDIR/${tname}.s32o" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
            $LIBC_OBJS
        if [[ ! -s "$WORKDIR/${tname}.s32x" ]]; then
            printf "  %-30s FAIL (link)\n" "$tname:"
            tail -n 20 "$WORKDIR/${tname}-ld.log" >&2
            FAIL=$((FAIL + 1))
            continue
        fi

        # Run
        set +e
        run_exe_rc "$WORKDIR/${tname}.s32x" "$WORKDIR/${tname}-run.log"
        RUN_RC=$?
        set -e
        if [[ "$RUN_RC" -eq 0 ]]; then
            printf "  %-30s PASS\n" "$tname:"
            PASS=$((PASS + 1))
        else
            printf "  %-30s FAIL (rc=%d)\n" "$tname:" "$RUN_RC"
            cat "$WORKDIR/${tname}-run.log" >&2
            FAIL=$((FAIL + 1))
        fi
    done
fi

# ============================================================
# Step 3d: Fixed-point gate (optional slow test)
# Build gen2 with gen1, then gen3 with gen2, and require gen2 == gen3.
# ============================================================
if [[ "$RUN_FIXED_POINT" -eq 1 && -s "$S13CC_EXE" ]]; then
    echo ""
    echo "=== Step 3d: Fixed-point gate (gen2 == gen3) ==="
    TOTAL=$((TOTAL + 1))
    EXEC_TIMEOUT=300

    run_exe "$S13CC_EXE" "$WORKDIR/fp-gen2-compile.log" "$S13CC_SRC" "$WORKDIR/fp-gen2.s"
    if [[ ! -s "$WORKDIR/fp-gen2.s" ]]; then
        printf "  %-30s FAIL (compile)\n" "fixed-point:"
        FAIL=$((FAIL + 1))
    else
        if grep -Eq '^hir_burg|^hir_burg_select|^hir_imm_sel|^hir_iconst_use|^hir_codegen_li|^hir_iconst_nonimm_top_ops' "$WORKDIR/fp-gen2-compile.log"; then
            echo "  gen2 self-compile stats:"
            grep -E '^hir_burg|^hir_imm_sel|^hir_iconst_use|^hir_codegen_li|^hir_iconst_nonimm_top_ops|^  ' "$WORKDIR/fp-gen2-compile.log" | head -n 36
        fi
        run_exe "$AS_EXE" "$WORKDIR/fp-gen2-assemble.log" "$WORKDIR/fp-gen2.s" "$WORKDIR/fp-gen2.s32o"
        if [[ ! -s "$WORKDIR/fp-gen2.s32o" ]]; then
            printf "  %-30s FAIL (assemble)\n" "fixed-point:"
            FAIL=$((FAIL + 1))
        else
            run_exe "$LD_EXE" "$WORKDIR/fp-gen2-link.log" \
                -o "$WORKDIR/fp-gen2.s32x" --mmio 64K \
                "$RUNTIME_CRT0" "$WORKDIR/fp-gen2.s32o" "$STAGE4_LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
                $STAGE4_LIBC_OBJS
            if [[ ! -s "$WORKDIR/fp-gen2.s32x" ]]; then
                printf "  %-30s FAIL (link)\n" "fixed-point:"
                FAIL=$((FAIL + 1))
            else
                run_exe "$WORKDIR/fp-gen2.s32x" "$WORKDIR/fp-gen3-compile.log" "$S13CC_SRC" "$WORKDIR/fp-gen3.s"
                if [[ ! -s "$WORKDIR/fp-gen3.s" ]]; then
                    printf "  %-30s FAIL (gen3 compile)\n" "fixed-point:"
                    FAIL=$((FAIL + 1))
                else
                    if grep -Eq '^hir_burg|^hir_burg_select|^hir_imm_sel|^hir_iconst_use|^hir_codegen_li|^hir_iconst_nonimm_top_ops' "$WORKDIR/fp-gen3-compile.log"; then
                        echo "  gen3 self-compile stats:"
                        grep -E '^hir_burg|^hir_imm_sel|^hir_iconst_use|^hir_codegen_li|^hir_iconst_nonimm_top_ops|^  ' "$WORKDIR/fp-gen3-compile.log" | head -n 36
                    fi
                    run_exe "$AS_EXE" "$WORKDIR/fp-gen3-assemble.log" "$WORKDIR/fp-gen3.s" "$WORKDIR/fp-gen3.s32o"
                    if [[ ! -s "$WORKDIR/fp-gen3.s32o" ]]; then
                        printf "  %-30s FAIL (gen3 assemble)\n" "fixed-point:"
                        FAIL=$((FAIL + 1))
                    else
                        run_exe "$LD_EXE" "$WORKDIR/fp-gen3-link.log" \
                            -o "$WORKDIR/fp-gen3.s32x" --mmio 64K \
                            "$RUNTIME_CRT0" "$WORKDIR/fp-gen3.s32o" "$STAGE4_LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
                            $STAGE4_LIBC_OBJS
                        if [[ ! -s "$WORKDIR/fp-gen3.s32x" ]]; then
                            printf "  %-30s FAIL (gen3 link)\n" "fixed-point:"
                            FAIL=$((FAIL + 1))
                        elif cmp -s "$WORKDIR/fp-gen2.s32x" "$WORKDIR/fp-gen3.s32x"; then
                            printf "  %-30s PASS\n" "fixed-point:"
                            PASS=$((PASS + 1))
                        else
                            printf "  %-30s FAIL (gen2 != gen3)\n" "fixed-point:"
                            echo "    gen2: $(wc -c < "$WORKDIR/fp-gen2.s32x") bytes" >&2
                            echo "    gen3: $(wc -c < "$WORKDIR/fp-gen3.s32x") bytes" >&2
                            FAIL=$((FAIL + 1))
                        fi
                    fi
                fi
            fi
        fi
    fi
    EXEC_TIMEOUT="$SAVED_TIMEOUT"
fi

# ============================================================
# Step 3b: Semantic equivalence smoke (stage04 vs stage05 cc)
# ============================================================
if [[ -s "$S13CC_EXE" ]]; then
    echo ""
    echo "=== Step 3b: Semantic equivalence (stage04 vs s13cc) ==="

    # Deterministic no-libc cases.
    # Compare observable behavior (program return code) between compilers.
    cat > "$WORKDIR/sem_eq_01.c" <<'EOF'
int main(void){int x;x=0;for(x=0;x<100;x=x+1){}return x-100;}
EOF
    cat > "$WORKDIR/sem_eq_02.c" <<'EOF'
int main(void){int a;a=7;if((a*8)+(a*4)!=(a<<3)+(a<<2))return 1;return 0;}
EOF
    cat > "$WORKDIR/sem_eq_03.c" <<'EOF'
int f(int x){if(x<2)return x;return f(x-1)+f(x-2);} int main(void){return f(8)-21;}
EOF
    cat > "$WORKDIR/sem_eq_04.c" <<'EOF'
int main(void){int x;x=12345; if((x&-1)!=x)return 1; if((x|-1)!=-1)return 2; if((x^-1)!=~x)return 3; return 0;}
EOF
    cat > "$WORKDIR/sem_eq_05.c" <<'EOF'
int main(void){int i;int s;i=0;s=0;while(i<50){s=s+i;i=i+1;}if(s!=1225)return 1;return 0;}
EOF
    cat > "$WORKDIR/sem_eq_06.c" <<'EOF'
int main(void){int x;x=3; x=(x+5)+9; return x-17;}
EOF
    cat > "$WORKDIR/sem_eq_07.c" <<'EOF'
int g(int x){int i;int s;s=0;for(i=0;i<20;i=i+1){s=s+((x*3)+5);}return s;} int main(void){return g(7)-520;}
EOF
    cat > "$WORKDIR/sem_eq_08.c" <<'EOF'
int main(void){int i;int j;int s;int x;x=9;s=0;for(i=0;i<6;i=i+1){for(j=0;j<5;j=j+1){s=s+((x*4)-3)+i;}}return s-1065;}
EOF
    cat > "$WORKDIR/sem_eq_09.c" <<'EOF'
int side; int touch(int x){side=side+1; return x+side;} int main(void){int i;int s;side=0;s=0;for(i=0;i<10;i=i+1){s=s+touch(3);} if(side!=10)return 1; return s-85;}
EOF
    cat > "$WORKDIR/sem_eq_10.c" <<'EOF'
int g0; int main(void){int i;int s;g0=1;s=0;for(i=0;i<8;i=i+1){s=s+g0; if(i==3) g0=2;} return s-12;}
EOF
    cat > "$WORKDIR/sem_eq_11.c" <<'EOF'
int main(void){int i;int a;int b;a=1;b=2;for(i=0;i<50;i=i+1){if((i&1)==0)a=a+b;else b=b+a; if(a<0)a=a&32767; if(b<0)b=b&32767;}return (a^b)&255;}
EOF
    cat > "$WORKDIR/sem_eq_12.c" <<'EOF'
int main(void){int i;int x;int y;x=1234;y=77;for(i=0;i<40;i=i+1){x=((x<<2)+(x>>3))-(y<<1); y=(y+3)^(x&31);}return (x+y)&255;}
EOF

    for src in \
        "$WORKDIR/sem_eq_01.c" \
        "$WORKDIR/sem_eq_02.c" \
        "$WORKDIR/sem_eq_03.c" \
        "$WORKDIR/sem_eq_04.c" \
        "$WORKDIR/sem_eq_05.c" \
        "$WORKDIR/sem_eq_06.c"; do
        name="$(basename "$src" .c)"
        TOTAL=$((TOTAL + 1))

        EXE_A=$(compile_and_link "${name}-s4" "$src" "$STAGE4_CC" "$AS_EXE" "$LD_EXE") || {
            printf "  %-30s FAIL (stage04 build)\n" "${name}:"
            FAIL=$((FAIL + 1))
            continue
        }
        EXE_B=$(compile_and_link "${name}-s5" "$src" "$S13CC_EXE" "$AS_EXE" "$LD_EXE") || {
            printf "  %-30s FAIL (s13cc build)\n" "${name}:"
            FAIL=$((FAIL + 1))
            continue
        }

        set +e
        run_exe_rc "$EXE_A" "$WORKDIR/${name}-s4.run.log"
        RC_A=$?
        run_exe_rc "$EXE_B" "$WORKDIR/${name}-s5.run.log"
        RC_B=$?
        set -e

        if [[ "$RC_A" -eq 96 ]]; then RC_A=0; fi
        if [[ "$RC_B" -eq 96 ]]; then RC_B=0; fi

        if [[ "$RC_A" -eq "$RC_B" ]]; then
            printf "  %-30s PASS (rc=%d)\n" "${name}:" "$RC_A"
            PASS=$((PASS + 1))
        else
            printf "  %-30s FAIL (s4=%d, s5=%d)\n" "${name}:" "$RC_A" "$RC_B"
            echo "    stage04 run log:" >&2
            tail -n 20 "$WORKDIR/${name}-s4.run.log" >&2 || true
            echo "    stage05 run log:" >&2
            tail -n 20 "$WORKDIR/${name}-s5.run.log" >&2 || true
            FAIL=$((FAIL + 1))
        fi
    done
fi

# ============================================================
# Step 3c: Optimizer regression pack (stage04 vs stage05 cc)
# ============================================================
if [[ -s "$S13CC_EXE" ]]; then
    echo ""
    echo "=== Step 3c: Optimizer regression pack ==="

    for src in \
        "$WORKDIR/sem_eq_07.c" \
        "$WORKDIR/sem_eq_08.c" \
        "$WORKDIR/sem_eq_09.c" \
        "$WORKDIR/sem_eq_10.c" \
        "$WORKDIR/sem_eq_11.c" \
        "$WORKDIR/sem_eq_12.c"; do
        name="$(basename "$src" .c)"
        TOTAL=$((TOTAL + 1))

        EXE_A=$(compile_and_link "${name}-s4" "$src" "$STAGE4_CC" "$AS_EXE" "$LD_EXE") || {
            printf "  %-30s FAIL (stage04 build)\n" "${name}:"
            FAIL=$((FAIL + 1))
            continue
        }
        EXE_B=$(compile_and_link "${name}-s5" "$src" "$S13CC_EXE" "$AS_EXE" "$LD_EXE") || {
            printf "  %-30s FAIL (s13cc build)\n" "${name}:"
            FAIL=$((FAIL + 1))
            continue
        }

        set +e
        run_exe_rc "$EXE_A" "$WORKDIR/${name}-s4.run.log"
        RC_A=$?
        run_exe_rc "$EXE_B" "$WORKDIR/${name}-s5.run.log"
        RC_B=$?
        set -e

        if [[ "$RC_A" -eq 96 ]]; then RC_A=0; fi
        if [[ "$RC_B" -eq 96 ]]; then RC_B=0; fi

        if [[ "$RC_A" -eq "$RC_B" ]]; then
            printf "  %-30s PASS (rc=%d)\n" "${name}:" "$RC_A"
            PASS=$((PASS + 1))
        else
            printf "  %-30s FAIL (s4=%d, s5=%d)\n" "${name}:" "$RC_A" "$RC_B"
            echo "    stage04 run log:" >&2
            tail -n 20 "$WORKDIR/${name}-s4.run.log" >&2 || true
            echo "    stage05 run log:" >&2
            tail -n 20 "$WORKDIR/${name}-s5.run.log" >&2 || true
            FAIL=$((FAIL + 1))
        fi
    done
fi

# ============================================================
# Step 4: Build tools with stage04's s12cc
# ============================================================
echo ""
echo "=== Step 4: Build stage05 tools ==="

S13_AS_SRC="$SCRIPT_DIR/tools/s32-as.c"
S13_AR_SRC="$SCRIPT_DIR/tools/s32-ar.c"
S13_LD_SRC="$SCRIPT_DIR/tools/s32-ld.c"

# Build assembler
TOTAL=$((TOTAL + 1))
EXEC_TIMEOUT=300
run_exe "$STAGE4_CC" "$WORKDIR/s32-as-compile.log" "$S13_AS_SRC" "$WORKDIR/s32-as.s"
if [[ ! -s "$WORKDIR/s32-as.s" ]]; then
    printf "  %-30s FAIL (compile)\n" "s32-as-build:"
    tail -n 20 "$WORKDIR/s32-as-compile.log" >&2
    FAIL=$((FAIL + 1))
else
    run_exe "$AS_EXE" "$WORKDIR/s32-as-assemble.log" "$WORKDIR/s32-as.s" "$WORKDIR/s32-as.s32o"
    if [[ ! -s "$WORKDIR/s32-as.s32o" ]]; then
        printf "  %-30s FAIL (assemble)\n" "s32-as-build:"
        FAIL=$((FAIL + 1))
    else
        run_exe "$LD_EXE" "$WORKDIR/s32-as-link.log" \
            -o "$WORKDIR/s32-as.s32x" --mmio 64K \
            "$RUNTIME_CRT0" "$WORKDIR/s32-as.s32o" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
            $LIBC_OBJS
        if [[ ! -s "$WORKDIR/s32-as.s32x" ]]; then
            printf "  %-30s FAIL (link)\n" "s32-as-build:"
            FAIL=$((FAIL + 1))
        else
            printf "  %-30s PASS\n" "s32-as-build:"
            PASS=$((PASS + 1))
        fi
    fi
fi

# Build archiver
TOTAL=$((TOTAL + 1))
run_exe "$STAGE4_CC" "$WORKDIR/s32-ar-compile.log" "$S13_AR_SRC" "$WORKDIR/s32-ar.s"
if [[ ! -s "$WORKDIR/s32-ar.s" ]]; then
    printf "  %-30s FAIL (compile)\n" "s32-ar-build:"
    tail -n 20 "$WORKDIR/s32-ar-compile.log" >&2
    FAIL=$((FAIL + 1))
else
    run_exe "$AS_EXE" "$WORKDIR/s32-ar-assemble.log" "$WORKDIR/s32-ar.s" "$WORKDIR/s32-ar.s32o"
    if [[ ! -s "$WORKDIR/s32-ar.s32o" ]]; then
        printf "  %-30s FAIL (assemble)\n" "s32-ar-build:"
        FAIL=$((FAIL + 1))
    else
        run_exe "$LD_EXE" "$WORKDIR/s32-ar-link.log" \
            -o "$WORKDIR/s32-ar.s32x" --mmio 64K \
            "$RUNTIME_CRT0" "$WORKDIR/s32-ar.s32o" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
            $LIBC_OBJS
        if [[ ! -s "$WORKDIR/s32-ar.s32x" ]]; then
            printf "  %-30s FAIL (link)\n" "s32-ar-build:"
            FAIL=$((FAIL + 1))
        else
            printf "  %-30s PASS\n" "s32-ar-build:"
            PASS=$((PASS + 1))
        fi
    fi
fi

# Build linker
TOTAL=$((TOTAL + 1))
run_exe "$STAGE4_CC" "$WORKDIR/s32-ld-compile.log" "$S13_LD_SRC" "$WORKDIR/s32-ld.s"
if [[ ! -s "$WORKDIR/s32-ld.s" ]]; then
    printf "  %-30s FAIL (compile)\n" "s32-ld-build:"
    tail -n 20 "$WORKDIR/s32-ld-compile.log" >&2
    FAIL=$((FAIL + 1))
else
    run_exe "$AS_EXE" "$WORKDIR/s32-ld-assemble.log" "$WORKDIR/s32-ld.s" "$WORKDIR/s32-ld.s32o"
    if [[ ! -s "$WORKDIR/s32-ld.s32o" ]]; then
        printf "  %-30s FAIL (assemble)\n" "s32-ld-build:"
        FAIL=$((FAIL + 1))
    else
        run_exe "$LD_EXE" "$WORKDIR/s32-ld-link.log" \
            -o "$WORKDIR/s32-ld.s32x" --mmio 64K \
            "$RUNTIME_CRT0" "$WORKDIR/s32-ld.s32o" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
            $LIBC_OBJS
        if [[ ! -s "$WORKDIR/s32-ld.s32x" ]]; then
            printf "  %-30s FAIL (link)\n" "s32-ld-build:"
            FAIL=$((FAIL + 1))
        else
            printf "  %-30s PASS\n" "s32-ld-build:"
            PASS=$((PASS + 1))
        fi
    fi
fi
EXEC_TIMEOUT="$SAVED_TIMEOUT"

# ============================================================
# Step 5: End-to-end toolchain test
# Use s13cc + stage05 tools to compile/assemble/link/run a program
# ============================================================
S13_AS_EXE="$WORKDIR/s32-as.s32x"
S13_AR_EXE="$WORKDIR/s32-ar.s32x"
S13_LD_EXE="$WORKDIR/s32-ld.s32x"

if [[ -s "$S13CC_EXE" && -s "$S13_AS_EXE" && -s "$S13_LD_EXE" ]]; then
    echo ""
    echo "=== Step 5: End-to-end toolchain test (s13cc + stage05 tools) ==="

    # Test: compile test_spike.c with s13cc, assemble with stage05 AS, link with stage05 LD
    for tst in test_spike test_phase2 test_phase3; do
        TST_SRC="$TESTS_DIR/${tst}.c"
        [[ -f "$TST_SRC" ]] || continue
        TOTAL=$((TOTAL + 1))

        # Compile with s13cc
        run_exe "$S13CC_EXE" "$WORKDIR/e2e-${tst}-cc.log" "$TST_SRC" "$WORKDIR/e2e-${tst}.s"
        if [[ ! -s "$WORKDIR/e2e-${tst}.s" ]]; then
            printf "  %-30s FAIL (compile)\n" "e2e-${tst}:"
            FAIL=$((FAIL + 1))
            continue
        fi

        # Assemble with stage05 AS
        run_exe "$S13_AS_EXE" "$WORKDIR/e2e-${tst}-as.log" "$WORKDIR/e2e-${tst}.s" "$WORKDIR/e2e-${tst}.s32o"
        if [[ ! -s "$WORKDIR/e2e-${tst}.s32o" ]]; then
            printf "  %-30s FAIL (assemble)\n" "e2e-${tst}:"
            FAIL=$((FAIL + 1))
            continue
        fi

        # Link with stage05 LD
        run_exe "$S13_LD_EXE" "$WORKDIR/e2e-${tst}-ld.log" \
            -o "$WORKDIR/e2e-${tst}.s32x" --mmio 64K \
            "$RUNTIME_CRT0" "$WORKDIR/e2e-${tst}.s32o" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
            $LIBC_OBJS
        if [[ ! -s "$WORKDIR/e2e-${tst}.s32x" ]]; then
            printf "  %-30s FAIL (link)\n" "e2e-${tst}:"
            FAIL=$((FAIL + 1))
            continue
        fi

        # Run
        set +e
        run_exe_rc "$WORKDIR/e2e-${tst}.s32x" "$WORKDIR/e2e-${tst}-run.log"
        RUN_RC=$?
        set -e
        if [[ "$RUN_RC" -eq 0 ]]; then
            printf "  %-30s PASS\n" "e2e-${tst}:"
            PASS=$((PASS + 1))
        else
            printf "  %-30s FAIL (rc=%d)\n" "e2e-${tst}:" "$RUN_RC"
            cat "$WORKDIR/e2e-${tst}-run.log" >&2
            FAIL=$((FAIL + 1))
        fi
    done

    # Test: use stage05 archiver to create a libc archive, then link with it
    if [[ -s "$S13_AR_EXE" ]]; then
        TOTAL=$((TOTAL + 1))
        echo ""
        echo "  --- Archive test: s32-ar creates libc.s32a, link with it ---"

        # Create archive from libc objects
        run_exe "$S13_AR_EXE" "$WORKDIR/e2e-ar.log" \
            rcs "$WORKDIR/e2e-libc.s32a" $LIBC_OBJS
        if [[ ! -s "$WORKDIR/e2e-libc.s32a" ]]; then
            printf "  %-30s FAIL (archive)\n" "e2e-archive:"
            tail -n 20 "$WORKDIR/e2e-ar.log" >&2
            FAIL=$((FAIL + 1))
        else
            # Compile test_spike with s13cc, assemble with stage05 AS, link with archive
            run_exe "$S13CC_EXE" "$WORKDIR/e2e-ar-cc.log" "$TESTS_DIR/test_spike.c" "$WORKDIR/e2e-ar-test.s"
            run_exe "$S13_AS_EXE" "$WORKDIR/e2e-ar-as.log" "$WORKDIR/e2e-ar-test.s" "$WORKDIR/e2e-ar-test.s32o"
            run_exe "$S13_LD_EXE" "$WORKDIR/e2e-ar-ld.log" \
                -o "$WORKDIR/e2e-ar-test.s32x" --mmio 64K \
                "$RUNTIME_CRT0" "$WORKDIR/e2e-ar-test.s32o" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
                "$WORKDIR/e2e-libc.s32a"
            if [[ ! -s "$WORKDIR/e2e-ar-test.s32x" ]]; then
                printf "  %-30s FAIL (link with archive)\n" "e2e-archive:"
                FAIL=$((FAIL + 1))
            else
                set +e
                run_exe_rc "$WORKDIR/e2e-ar-test.s32x" "$WORKDIR/e2e-ar-run.log"
                RUN_RC=$?
                set -e
                if [[ "$RUN_RC" -eq 0 ]]; then
                    printf "  %-30s PASS\n" "e2e-archive:"
                    PASS=$((PASS + 1))
                else
                    printf "  %-30s FAIL (rc=%d)\n" "e2e-archive:" "$RUN_RC"
                    cat "$WORKDIR/e2e-ar-run.log" >&2
                    FAIL=$((FAIL + 1))
                fi
            fi
        fi
    fi
fi

# ============================================================
# Final report
# ============================================================
echo ""
if [[ "$FAIL" -eq 0 ]]; then
    echo "OK: stage05 ($PASS/$TOTAL tests passed)"
else
    echo "FAIL: stage05 ($PASS/$TOTAL tests passed, $FAIL failed)" >&2
    exit 1
fi

if [[ "$SHAPE_WARN" -gt 0 ]]; then
    if [[ -f "$SHAPE_WARN_FILE" ]]; then
        SHAPE_WARN=$(wc -l < "$SHAPE_WARN_FILE")
    fi
    echo "WARN: shape checks reported opportunities in $SHAPE_WARN compilations"
    if [[ "$STRICT_SHAPE" -eq 0 ]]; then
        echo "      rerun with --strict-shape to make these fatal"
    fi
fi

echo "Artifacts: $WORKDIR"
