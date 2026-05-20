#!/usr/bin/env bash
# gen_ref.sh — assemble tests/ref.s and extract the .text bytes into ref.bin
#
# The reference bytes must come from an ELF object so `objcopy -j .text`
# selects the right section.  On Linux that's gas; on macOS we use clang
# with an explicit aarch64-linux-gnu triple (the system `as` produces
# Mach-O, where the section is __text).
set -euo pipefail
cd "$(dirname "$0")/.."

case "$(uname -s)" in
    Darwin)
        "${CC:-clang}" -c -target aarch64-linux-gnu -x assembler \
            -o tests/ref.o tests/ref.s
        ;;
    *)
        as -march=armv8-a -o tests/ref.o tests/ref.s
        ;;
esac

# Pick an objcopy.  GNU objcopy on Linux; llvm-objcopy (compatible with
# GNU objcopy) elsewhere.  Falls back to the in-tree LLVM build that
# ships with this dev environment.
if   command -v objcopy      >/dev/null 2>&1; then OBJCOPY=objcopy
elif command -v llvm-objcopy >/dev/null 2>&1; then OBJCOPY=llvm-objcopy
elif [ -x "$HOME/llvm-project/build/bin/llvm-objcopy" ]; then
    OBJCOPY="$HOME/llvm-project/build/bin/llvm-objcopy"
else
    echo "gen_ref.sh: need objcopy or llvm-objcopy on PATH" >&2
    exit 1
fi

"$OBJCOPY" -O binary -j .text tests/ref.o tests/ref.bin
echo "wrote tests/ref.bin ($(wc -c < tests/ref.bin | tr -d ' ') bytes)"
