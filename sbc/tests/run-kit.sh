#!/bin/bash
# The two BASIC compilers as the self-hosted kit carries them, built by
# stage08 alone (../build-kit.sh), with nothing of the host's or LLVM's in
# a compiled program:
#
#   1. byte for byte: s32-bc.s32x writes the assembly the host s32-bc
#      writes, s32-sbc.s32x the C the host s32-sbc writes, and the same
#      programs are refused with the same words, over the whole corpus.
#      The compilers are themselves large programs stage08 has seen nowhere
#      else, and this is the sharp test of them.
#   2. the oracle: every program compiled through docker/bin/s32kbasic,
#      with stage08's libc and the interpreter's run time built by stage08,
#      prints what the clang-built interpreter prints (run-oracle.py --kit).
#
#   sbc/tests/run-kit.sh [run-oracle.py arguments]
#
#   KIT   a kit to test (~/s32x, or /opt/slow32/kit); default: one staged
#         here from the tree's stage08 and build-kit.sh
#   EMU   the engine the tools run on (default tools/dbt/slow32-dbt)
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
SBC_DIR="$(dirname "$HERE")"
ROOT="$(dirname "$SBC_DIR")"
export EMU="${EMU:-$ROOT/tools/dbt/slow32-dbt}"
mkdir -p "$SBC_DIR/out"
W=$(mktemp -d "$SBC_DIR/out/kit.XXXXXX") || exit 2
trap 'rm -rf "${W:?}"' EXIT
fail=0

if [ -z "${KIT:-}" ]; then
    T="$ROOT/selfhost/stage08"
    KIT="$W/kit"
    mkdir -p "$KIT/selfhost"
    cp "$T/cc.s32x" "$T/s32-as.s32x" "$T/s32-ld.s32x" "$T/s32-ar.s32x" "$KIT/"
    cp -R "$T/include" "$KIT/selfhost/include"
    cp "$T/lib/crt0.s32o" "$KIT/selfhost/"
    members=()
    for o in "$T"/lib/*.s32o; do
        [ "$(basename "$o")" = crt0.s32o ] || members+=("$o")
    done
    "$EMU" "$T/s32-ar.s32x" rc "$KIT/selfhost/libc.s32a" "${members[@]}" >/dev/null
    KIT_TOOLS="$KIT" KIT_RT="$KIT/selfhost" bash "$SBC_DIR/build-kit.sh" "$KIT" >/dev/null || exit 1
fi
export KIT
echo "kit: $KIT, on $(basename "$EMU")"

bash "$SBC_DIR/build-bc.sh" >/dev/null && bash "$SBC_DIR/build.sh" >/dev/null || exit 1

echo "=== byte for byte, host against self-hosted"
python3 "$HERE/kit-bytes.py" "$KIT" "$W/bytes" || fail=1

echo "=== s32-bc from the kit, held to the interpreter"
python3 "$HERE/run-oracle.py" --kit bc "$@" | grep -E "DIFFER|TIMEOUT|^AGREE" || fail=1
[ "${PIPESTATUS[0]}" = 0 ] || fail=1

echo "=== s32-sbc from the kit, held to the interpreter"
python3 "$HERE/run-oracle.py" --kit sbc "$@" | grep -E "DIFFER|TIMEOUT|NOCOMPILE|^AGREE" || fail=1
[ "${PIPESTATUS[0]}" = 0 ] || fail=1
exit "$fail"
