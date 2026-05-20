#!/bin/bash
# Differential test harness: cc-x64 (--hir) vs x86_64-linux-gnu-gcc.
#
# For each .c file given (or every .c found in default corpus dirs):
#   - Compile with cc-x64 --hir into an x86-64 ELF
#   - Compile with x86_64-linux-gnu-gcc -O2 -static into an x86-64 ELF
#   - Run both under qemu-x86_64-static -s 64M, capture exit code + stdout
#   - Report PASS / MISMATCH / BUILD-FAIL
#
# Exit code 0 if all PASS, 1 otherwise.

set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
TOP="$(cd "$HERE/.." && pwd)"

CC_X64="$TOP/out/cc-x64"
LD_X64="$TOP/out/ld-x64"
CRT0="$TOP/out/crt0.o"
LIBC="$TOP/out/libc_x64.a"

GCC="${GCC:-x86_64-linux-gnu-gcc}"
QEMU="${QEMU:-qemu-x86_64-static}"
QEMU_STACK="${QEMU_STACK:-67108864}"

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

pass=0
mismatch=0
build_fail_cc=0
build_fail_gcc=0
total=0
first_failure=""

run_one() {
    local src="$1"
    local name="$(basename "$src" .c)"
    total=$((total + 1))

    local cc_obj="$WORK/$name.cc.o"
    local cc_exe="$WORK/$name.cc"
    local gcc_exe="$WORK/$name.gcc"
    local cc_log="$WORK/$name.cc.log"
    local gcc_log="$WORK/$name.gcc.log"

    # Build with cc-x64
    if ! "$CC_X64" --hir -I . -c "$src" -o "$cc_obj" > "$cc_log" 2>&1; then
        build_fail_cc=$((build_fail_cc + 1))
        [ -z "$first_failure" ] && first_failure="BUILD-FAIL-CC: $name ($(tail -1 "$cc_log"))"
        echo "  BUILD-FAIL-CC  $name"
        return
    fi
    if ! "$LD_X64" -o "$cc_exe" "$CRT0" "$cc_obj" "$LIBC" >> "$cc_log" 2>&1; then
        build_fail_cc=$((build_fail_cc + 1))
        [ -z "$first_failure" ] && first_failure="BUILD-FAIL-CC-LINK: $name"
        echo "  BUILD-FAIL-CC  $name (link)"
        return
    fi

    # Build with cross gcc
    if ! "$GCC" -O2 -static -w -o "$gcc_exe" "$src" > "$gcc_log" 2>&1; then
        build_fail_gcc=$((build_fail_gcc + 1))
        [ -z "$first_failure" ] && first_failure="BUILD-FAIL-GCC: $name ($(tail -1 "$gcc_log"))"
        echo "  BUILD-FAIL-GCC $name"
        return
    fi

    # Run both
    local cc_stdout="$WORK/$name.cc.out"
    local gcc_stdout="$WORK/$name.gcc.out"
    "$QEMU" -s "$QEMU_STACK" "$cc_exe" > "$cc_stdout" 2>/dev/null
    local cc_rc=$?
    "$QEMU" -s "$QEMU_STACK" "$gcc_exe" > "$gcc_stdout" 2>/dev/null
    local gcc_rc=$?

    if [ "$cc_rc" = "$gcc_rc" ] && cmp -s "$cc_stdout" "$gcc_stdout"; then
        pass=$((pass + 1))
        echo "  PASS           $name  (exit=$cc_rc)"
    else
        mismatch=$((mismatch + 1))
        local detail="exit cc=$cc_rc gcc=$gcc_rc"
        if ! cmp -s "$cc_stdout" "$gcc_stdout"; then
            detail="$detail  stdout differs"
        fi
        echo "  MISMATCH       $name  ($detail)"
        if [ -z "$first_failure" ]; then
            first_failure="MISMATCH: $name  cc=$cc_rc gcc=$gcc_rc"
            if ! cmp -s "$cc_stdout" "$gcc_stdout"; then
                first_failure="$first_failure
    cc stdout : $(head -c 80 "$cc_stdout")
    gcc stdout: $(head -c 80 "$gcc_stdout")"
            fi
        fi
    fi
}

# Sanity checks
for tool in "$CC_X64" "$LD_X64" "$CRT0" "$LIBC"; do
    [ -e "$tool" ] || { echo "Missing: $tool — run 'make' first"; exit 2; }
done
command -v "$GCC" >/dev/null || { echo "Missing: $GCC"; exit 2; }
command -v "$QEMU" >/dev/null || { echo "Missing: $QEMU"; exit 2; }

# Collect inputs
#
# Default: just diff-test/corpus/ (portable C, designed for differential
# comparison).  Pass --with-tests to also probe ../tests/ — but expect
# build-fails and mismatches there since many of those tests use
# cc-x64-specific calling conventions (`int printf(char *fmt, int a0, ...
# int a7);`) that gcc legitimately rejects or runs differently.
include_tests=0
inputs=()
while [ $# -gt 0 ]; do
    case "$1" in
        --with-tests) include_tests=1 ;;
        *) inputs+=("$1") ;;
    esac
    shift
done
if [ ${#inputs[@]} -eq 0 ]; then
    for f in "$HERE/corpus"/*.c; do
        [ -e "$f" ] && inputs+=("$f")
    done
    if [ "$include_tests" = "1" ]; then
        for f in "$TOP/tests"/*.c; do
            [ -e "$f" ] && inputs+=("$f")
        done
    fi
fi

echo "=== diff-test: cc-x64 vs $GCC ==="
echo "  corpus: ${#inputs[@]} files"
echo ""
for src in "${inputs[@]}"; do
    run_one "$src"
done

echo ""
echo "=== summary ==="
echo "  total          : $total"
echo "  PASS           : $pass"
echo "  MISMATCH       : $mismatch"
echo "  BUILD-FAIL-CC  : $build_fail_cc"
echo "  BUILD-FAIL-GCC : $build_fail_gcc"
if [ -n "$first_failure" ]; then
    echo ""
    echo "  first failure:"
    echo "  $first_failure"
fi

[ "$mismatch" = "0" ] && exit 0 || exit 1
