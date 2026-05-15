#!/bin/bash
# Differential test harness: cc-a64 (--hir) vs host gcc.
#
# For each .c file (default: every .c in corpus/, which is symlinked
# to the shared diff-test corpus in ../stage07-cross/diff-test/corpus/):
#   - Compile with cc-a64 --hir into an aarch64 ELF
#   - Compile with host gcc -O2 -static into an aarch64 ELF
#   - Run both natively (we ARE on aarch64), capture exit code + stdout
#   - Report PASS / MISMATCH / BUILD-FAIL
#
# Skipped if `uname -m` isn't aarch64 — running cc-a64 outputs requires
# either a native aarch64 host or qemu-aarch64; the comparison binary
# (host gcc) wouldn't be aarch64 either, so the comparison is moot.
#
# Exit code 0 if all PASS, 1 otherwise, 2 if skipped.

set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
TOP="$(cd "$HERE/.." && pwd)"

CC_A64="$TOP/out/cc-a64"
LD_A64="$TOP/out/ld-a64"
CRT0="$TOP/out/crt0.o"
LIBC="$TOP/out/libc_a64.a"

GCC="${GCC:-gcc}"

# Architecture gate
host_arch="$(uname -m)"
if [ "$host_arch" != "aarch64" ] && [ "$host_arch" != "arm64" ]; then
    echo "diff-test (cc-a64): host arch is $host_arch, not aarch64 — skipping"
    exit 2
fi

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

    if ! "$CC_A64" --hir -I . -c "$src" -o "$cc_obj" > "$cc_log" 2>&1; then
        build_fail_cc=$((build_fail_cc + 1))
        [ -z "$first_failure" ] && first_failure="BUILD-FAIL-CC: $name ($(tail -1 "$cc_log"))"
        echo "  BUILD-FAIL-CC  $name"
        return
    fi
    if ! "$LD_A64" -o "$cc_exe" "$CRT0" "$cc_obj" "$LIBC" >> "$cc_log" 2>&1; then
        build_fail_cc=$((build_fail_cc + 1))
        [ -z "$first_failure" ] && first_failure="BUILD-FAIL-CC-LINK: $name"
        echo "  BUILD-FAIL-CC  $name (link)"
        return
    fi

    if ! "$GCC" -O2 -static -w -o "$gcc_exe" "$src" > "$gcc_log" 2>&1; then
        build_fail_gcc=$((build_fail_gcc + 1))
        [ -z "$first_failure" ] && first_failure="BUILD-FAIL-GCC: $name ($(tail -1 "$gcc_log"))"
        echo "  BUILD-FAIL-GCC $name"
        return
    fi

    local cc_stdout="$WORK/$name.cc.out"
    local gcc_stdout="$WORK/$name.gcc.out"
    "$cc_exe" > "$cc_stdout" 2>/dev/null
    local cc_rc=$?
    "$gcc_exe" > "$gcc_stdout" 2>/dev/null
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

for tool in "$CC_A64" "$LD_A64" "$CRT0" "$LIBC"; do
    [ -e "$tool" ] || { echo "Missing: $tool — run 'make' first"; exit 2; }
done
command -v "$GCC" >/dev/null || { echo "Missing: $GCC"; exit 2; }

inputs=()
if [ $# -gt 0 ]; then
    inputs=("$@")
else
    for f in "$HERE/corpus"/*.c; do
        [ -e "$f" ] && inputs+=("$f")
    done
fi

echo "=== diff-test: cc-a64 vs $GCC ==="
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
