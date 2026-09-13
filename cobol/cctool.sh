# cctool.sh -- how cobol/ turns a C file into a SLOW-32 object.
#
# The tree's usual answer is the LLVM backend: $LLVM_BIN/clang -target
# slow32 and llc.  A machine that has the slow-32 tree but not an LLVM
# build still has a C compiler for SLOW-32 -- the self-hosted stage08
# cc.s32x in the runtime kit -- so fall back to it and run it under the
# emulator.  Objects from the two compilers share one ABI and link in
# any combination (selfhost/stage08/RUNTIME_KIT.md), with one asymmetry:
# clang inlines 64-bit multiply, so __muldi3 is in neither runtime
# archive while stage08 emits a call to it.  s32_cc_builtins names the
# object that supplies it, and is empty on the clang path.
#
# Sourced by build.sh and compile.sh; both already set HERE and ROOT.
#
# Overrides: LLVM_BIN, S32_KIT (the runtime kit, default ~/s32x),
# S32_EMU (the emulator); S32_AS and S32_RT_INCLUDE point at the
# assembler and the runtime headers when they are not at their tree
# paths (the slow32:cobol image installs them under /opt/slow32).

: "${LLVM_BIN:=$HOME/llvm-project/build/bin}"
: "${S32_KIT:=$HOME/s32x}"
: "${OPT:=-O1}"
: "${S32_AS:=$ROOT/tools/assembler/slow32asm}"
: "${S32_RT_INCLUDE:=$ROOT/runtime/include}"

if [ -x "$LLVM_BIN/clang" ]; then
    s32_cc_backend=llvm
else
    s32_cc_backend=selfhost
    if [ -z "${S32_EMU:-}" ]; then
        S32_EMU=$(command -v slow32-dbt 2>/dev/null || true)
    fi
    if [ -z "${S32_EMU:-}" ] || [ ! -x "$S32_EMU" ]; then
        for cand in "$HOME/bin/slow32-dbt" "$ROOT/tools/dbt/slow32-dbt" \
                    "$ROOT/tools/emulator/slow32-fast"; do
            [ -x "$cand" ] && { S32_EMU="$cand"; break; }
        done
    fi
    # The kit's cc.s32x, not selfhost/stage08/cc.s32x in the tree: the
    # in-tree copy is whatever was committed, and predates the argument
    # marshalling fix of 2026-08-25 (RUNTIME_KIT.md, "Kit vintage").
    # Having neither is not fatal here: a pure-COBOL build never asks for
    # a C compiler (the slow32:cobol image has none).  s32_cc_obj refuses
    # when it is actually asked.
    if [ ! -f "$S32_KIT/cc.s32x" ] || [ ! -x "${S32_EMU:-}" ]; then
        s32_cc_backend=none
    fi
fi

# s32_cc_obj out.s32o in.c [-Idir ...] -- compile and assemble one C file.
s32_cc_obj() {
    _o=$1; _c=$2; shift 2
    _base="${_o%.s32o}"
    if [ "$s32_cc_backend" = llvm ]; then
        "$LLVM_BIN/clang" -target slow32-unknown-none -S -emit-llvm $OPT \
            -nostdinc -fno-builtin -I"$S32_RT_INCLUDE" "$@" \
            "$_c" -o "$_base.ll"
        "$LLVM_BIN/llc" -mtriple=slow32-unknown-none "$_base.ll" -o "$_base.s"
    elif [ "$s32_cc_backend" = selfhost ]; then
        # cc.s32x narrates its optimiser and selector counters on
        # stderr; keep them for a failure and drop them otherwise.
        _log="$_base.cclog"
        if ! "$S32_EMU" "$S32_KIT/cc.s32x" -I"$S32_RT_INCLUDE" "$@" \
                "$_c" "$_base.s" >/dev/null 2>"$_log"; then
            cat "$_log" >&2; rm -f "$_log"; return 1
        fi
        rm -f "$_log"
    else
        echo "cctool: no C compiler for SLOW-32 (needed for $_c)." >&2
        echo "  no clang at $LLVM_BIN/clang, and the self-hosted fallback needs" >&2
        echo "  $S32_KIT/cc.s32x (set S32_KIT) and an emulator (set S32_EMU)." >&2
        echo "  In a container, .c inputs need slow32:toolchain, not slow32:cobol." >&2
        return 1
    fi
    "$S32_AS" "$_base.s" "$_o" >/dev/null
}

# s32_cc_builtins -- the object that closes __muldi3 on the selfhost
# path; prints nothing when clang built everything.
s32_cc_builtins() {
    [ "$s32_cc_backend" = selfhost ] || return 0
    _b="$ROOT/cobol/out/builtins64.s32o"
    _s="$ROOT/selfhost/stage08/builtins64.s"
    # rebuilt when the source is newer: an object older than the source
    # linked a day-old divider on Kagura and nothing noticed (GitHub #14)
    if [ ! -f "$_b" ] || [ "$_s" -nt "$_b" ]; then
        mkdir -p "$ROOT/cobol/out"
        "$S32_AS" "$_s" "$_b" >/dev/null
    fi
    echo "$_b"
}
