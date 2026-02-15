#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="${SELFHOST_ROOT:-$(cd "$SCRIPT_DIR/../.." && pwd)}"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${SELFHOST_EMU:-$ROOT_DIR/selfhost/v2/stage00/s32-emu}"
KERNEL="${SELFHOST_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${SELFHOST_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
ASM_FTH="${SELFHOST_ASM_FTH:-$ROOT_DIR/selfhost/v2/stage01/asm.fth}"
LINK_FTH="${SELFHOST_LINK_FTH:-$ROOT_DIR/selfhost/v2/stage03/link.fth}"

KEEP_ARTIFACTS=0
SKIP_BOOT=0

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--kernel <path>] [--keep-artifacts] [--skip-boot]

Build and boot a fully selfhosted Forth kernel image:
  1) Stage01 assembles:
     - selfhost/v2/stage01/crt0_minimal.s
     - selfhost/v2/stage01/mmio_minimal.s
     - forth/kernel.s
  2) Stage03 links those three .s32o files into kernel-selfhost.s32x
  3) Optional boot smoke: run "1 2 + . CR BYE" and require output "3"

Env overrides:
  SELFHOST_ROOT SELFHOST_EMU SELFHOST_KERNEL SELFHOST_PRELUDE
  SELFHOST_ASM_FTH SELFHOST_LINK_FTH
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --emu)
            shift
            [[ $# -gt 0 ]] || { echo "--emu requires a path" >&2; exit 2; }
            EMU="$1"
            ;;
        --kernel)
            shift
            [[ $# -gt 0 ]] || { echo "--kernel requires a path" >&2; exit 2; }
            KERNEL="$1"
            ;;
        --keep-artifacts) KEEP_ARTIFACTS=1 ;;
        --skip-boot) SKIP_BOOT=1 ;;
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

for req in "$EMU" "$KERNEL" "$PRELUDE" "$ASM_FTH" "$LINK_FTH"; do
    [[ -f "$req" ]] || { echo "Missing required file: $req" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage03-selfhost-kernel.XXXXXX)"
if [[ "$KEEP_ARTIFACTS" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
fi
SHORT_DIR="/tmp/s3k.$$"
rm -rf "$SHORT_DIR"
mkdir -p "$SHORT_DIR"
if [[ "$KEEP_ARTIFACTS" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR" "$SHORT_DIR"' EXIT
fi

run_forth() {
    local script_a="$1"
    local script_b="$2"
    local body="$3"
    local log="$4"

    set +e
    cat "$PRELUDE" "$script_a" "$script_b" - <<FTH | timeout 300 "$EMU" "$KERNEL" >"$log" 2>&1
$body
FTH
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "forth pipeline failed (rc=$rc)" >&2
        tail -n 80 "$log" >&2
        return 1
    fi
}

assemble_with_stage01() {
    local src="$1"
    local out="$2"
    local log="$3"

    run_forth "$ASM_FTH" /dev/null "S\" $src\" S\" $out\" ASSEMBLE
BYE" "$log"
    [[ -s "$out" ]] || {
        echo "assembler produced no output: $src" >&2
        tail -n 80 "$log" >&2
        return 1
    }
    if grep -q "FAILED:" "$log"; then
        echo "assembler failed: $src" >&2
        tail -n 80 "$log" >&2
        return 1
    fi
}

read_le32() {
    local file="$1"
    local off="$2"
    od -An -tu4 -N4 -j"$off" "$file" | tr -d ' \n'
}

echo "[1/3] Stage01 assemble prerequisites"
cp "$ROOT_DIR/selfhost/v2/stage01/crt0_minimal.s" "$SHORT_DIR/c0.s"
cp "$ROOT_DIR/selfhost/v2/stage01/mmio_minimal.s" "$SHORT_DIR/m0.s"
cp "$ROOT_DIR/forth/kernel.s" "$SHORT_DIR/k0.s"

CRT0_OBJ="$SHORT_DIR/c0.o"
MMIO_OBJ="$SHORT_DIR/m0.o"
KERNEL_OBJ="$SHORT_DIR/k0.o"

rm -f "$CRT0_OBJ" "$MMIO_OBJ" "$KERNEL_OBJ"
assemble_with_stage01 "$SHORT_DIR/c0.s" "$CRT0_OBJ" "$WORKDIR/crt0.log"
assemble_with_stage01 "$SHORT_DIR/m0.s" "$MMIO_OBJ" "$WORKDIR/mmio.log"
assemble_with_stage01 "$SHORT_DIR/k0.s" "$KERNEL_OBJ" "$WORKDIR/kernel.log"

echo "[2/3] Stage03 link selfhost kernel"
OUT_EXE="$SHORT_DIR/k0.x"
rm -f "$OUT_EXE"
run_forth "$LINK_FTH" /dev/null "LINK-INIT
S\" $CRT0_OBJ\" LINK-OBJ
S\" $KERNEL_OBJ\" LINK-OBJ
S\" $MMIO_OBJ\" LINK-OBJ
65536 LINK-MMIO
S\" $OUT_EXE\" LINK-EMIT
BYE" "$WORKDIR/link.log"
[[ -s "$OUT_EXE" ]] || { echo "linker produced no output" >&2; exit 1; }

code_limit="$(read_le32 "$OUT_EXE" 32)"
if [[ -z "$code_limit" || "$code_limit" -lt 1048576 ]]; then
    echo "linked image has invalid code_limit: ${code_limit:-<none>}" >&2
    exit 1
fi

if [[ "$SKIP_BOOT" -eq 0 ]]; then
    echo "[3/3] Boot smoke"
    set +e
    printf '1 2 + . CR BYE\n' | timeout 90 "$EMU" "$OUT_EXE" >"$WORKDIR/boot.log" 2>&1
    boot_rc=$?
    set -e
    if [[ "$boot_rc" -ne 0 && "$boot_rc" -ne 96 ]]; then
        echo "boot run failed (rc=$boot_rc)" >&2
        tail -n 80 "$WORKDIR/boot.log" >&2
        exit 1
    fi
    if ! grep -Eq '(^|[^0-9])3([^0-9]|$)' "$WORKDIR/boot.log"; then
        echo "boot smoke did not print expected result (3)" >&2
        tail -n 80 "$WORKDIR/boot.log" >&2
        exit 1
    fi
fi

cp "$CRT0_OBJ" "$WORKDIR/crt0_minimal.s32o"
cp "$MMIO_OBJ" "$WORKDIR/mmio_minimal.s32o"
cp "$KERNEL_OBJ" "$WORKDIR/kernel.s32o"
cp "$OUT_EXE" "$WORKDIR/kernel-selfhost.s32x"

echo "OK: selfhost kernel pipeline passed"
echo "Image: $WORKDIR/kernel-selfhost.s32x"
echo "Artifacts: $WORKDIR"
