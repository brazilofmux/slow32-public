#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"

LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
CLANG="${CLANG:-$LLVM_BIN/clang}"
EMU="${SELFHOST_EMU:-}"
SELF_CC="${SELF_CC:-$SCRIPT_DIR/cc.s32x}"
OPT="${OPT:--O2}"
KEEP=0
TESTS=()
LIST_FILE="${LIST_FILE:-$SCRIPT_DIR/tests/codegen_compare.list}"

choose_default_emu() {
    local dbt="$ROOT_DIR/tools/dbt/slow32-dbt"
    if [[ -x "$dbt" ]]; then
        printf '%s\n' "$dbt"
    else
        printf '%s\n' "$SELFHOST_DIR/stage00/s32-emu"
    fi
}

usage() {
    cat <<USAGE
Usage: $0 [options] [test.c ...]

Compare LLVM vs selfhost stage06 assembly shape.

Options:
  --clang <path>     Clang binary (default: \$LLVM_BIN/clang)
  --llvm-bin <dir>   LLVM bin dir (default: ~/llvm-project/build/bin)
  --self-cc <path>   Selfhost compiler exe (default: selfhost/stage06/cc.s32x)
  --emu <path>       Emulator for selfhost compiler (default: slow32-dbt or stage00/s32-emu)
  --opt <flag>       LLVM optimization level (default: -O2)
  --list <file>      File containing test paths (one per line)
  --keep             Keep temp workdir
  -h, --help         Show this help

If no test files are passed, defaults to:
  selfhost/stage06/tests/codegen_compare.list
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --clang) CLANG="$2"; shift 2 ;;
        --llvm-bin) LLVM_BIN="$2"; CLANG="$2/clang"; shift 2 ;;
        --self-cc) SELF_CC="$2"; shift 2 ;;
        --emu) EMU="$2"; shift 2 ;;
        --opt) OPT="$2"; shift 2 ;;
        --list) LIST_FILE="$2"; shift 2 ;;
        --keep) KEEP=1; shift ;;
        -h|--help) usage; exit 0 ;;
        --) shift; break ;;
        -*) echo "Unknown option: $1" >&2; usage; exit 2 ;;
        *) TESTS+=("$1"); shift ;;
    esac
done

while [[ $# -gt 0 ]]; do
    TESTS+=("$1")
    shift
done

if [[ ${#TESTS[@]} -eq 0 ]]; then
    [[ -f "$LIST_FILE" ]] || { echo "Missing list file: $LIST_FILE" >&2; exit 1; }
    while IFS= read -r line; do
        line="${line%%#*}"
        line="$(printf '%s' "$line" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')"
        [[ -n "$line" ]] || continue
        TESTS+=("$ROOT_DIR/$line")
    done < "$LIST_FILE"
fi

if [[ -z "$EMU" ]]; then
    EMU="$(choose_default_emu)"
fi

[[ -x "$CLANG" ]] || { echo "Missing clang: $CLANG" >&2; exit 1; }
[[ -f "$SELF_CC" ]] || { echo "Missing selfhost compiler: $SELF_CC" >&2; exit 1; }
[[ -f "$EMU" ]] || { echo "Missing emulator: $EMU" >&2; exit 1; }

WORKDIR="$(mktemp -d /tmp/llvm-selfhost-cmp.XXXXXX)"
if [[ "$KEEP" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
fi

asm_insn_count() {
    local asm="$1"
    awk '
    {
        line = $0
        sub(/;.*/, "", line)
        gsub(/^[[:space:]]+/, "", line)
        if (line == "") next
        if (line ~ /^[.]/) next
        if (line ~ /:$/) next
        split(line, t, /[[:space:]]+/)
        op = t[1]
        if (op == "") next
        if (op ~ /^[.]/) next
        c++
    }
    END { print c + 0 }
    ' "$asm"
}

asm_opcode_hist() {
    local asm="$1"
    awk '
    {
        line = $0
        sub(/;.*/, "", line)
        gsub(/^[[:space:]]+/, "", line)
        if (line == "" || line ~ /^[.]/ || line ~ /:$/) next
        split(line, t, /[[:space:]]+/)
        op = t[1]
        if (op == "" || op ~ /^[.]/) next
        cnt[op]++
    }
    END {
        for (k in cnt) printf "%8d %s\n", cnt[k], k
    }
    ' "$asm" | sort -rn | head -n 8
}

printf "LLVM vs Selfhost Assembly Comparison (%s)\n" "$OPT"
printf "clang:   %s\n" "$CLANG"
printf "selfcc:  %s\n" "$SELF_CC"
printf "emu:     %s\n" "$EMU"
printf "\n"

printf "%-28s %10s %10s %10s %10s %9s\n" \
    "Test" "LLVM_insn" "Self_insn" "LLVM_B" "Self_B" "Delta%"
printf "%-28s %10s %10s %10s %10s %9s\n" \
    "----------------------------" "----------" "----------" "----------" "----------" "---------"

for t in "${TESTS[@]}"; do
    if [[ ! -f "$t" ]]; then
        echo "skip: missing test file: $t" >&2
        continue
    fi
    base="$(basename "$t" .c)"
    llvm_asm="$WORKDIR/${base}.llvm.s"
    self_asm="$WORKDIR/${base}.self.s"

    tdir="$(cd "$(dirname "$t")" && pwd)"

    "$CLANG" -target slow32-unknown-none "$OPT" -ffreestanding -fno-builtin -S \
        -I"$tdir" "$t" -o "$llvm_asm"

    timeout "${EXEC_TIMEOUT:-1200}" "$EMU" "$SELF_CC" "$t" "$self_asm" >/dev/null 2>&1 || {
        echo "selfhost compile failed: $t" >&2
        continue
    }
    [[ -s "$self_asm" ]] || { echo "selfhost produced no asm: $t" >&2; continue; }

    li="$(asm_insn_count "$llvm_asm")"
    si="$(asm_insn_count "$self_asm")"
    lb="$(wc -c < "$llvm_asm")"
    sb="$(wc -c < "$self_asm")"
    delta="n/a"
    if [[ "$li" -gt 0 ]]; then
        delta="$(awk "BEGIN { printf \"%.1f\", (($si - $li) * 100.0) / $li }")"
    fi

    printf "%-28s %10d %10d %10d %10d %8s%%\n" \
        "$base" "$li" "$si" "$lb" "$sb" "$delta"

    {
        echo "== LLVM top ops =="
        asm_opcode_hist "$llvm_asm"
        echo
        echo "== Selfhost top ops =="
        asm_opcode_hist "$self_asm"
    } > "$WORKDIR/${base}.ops.txt"
done

printf "\nPer-test opcode summaries: %s/*.ops.txt\n" "$WORKDIR"
if [[ "$KEEP" -eq 1 ]]; then
    printf "Artifacts kept: %s\n" "$WORKDIR"
fi
