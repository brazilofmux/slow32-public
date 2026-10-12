#!/bin/bash
# Differential oracle: run an ISLISP program with Easy-ISLisp in a
# container, and print exactly what it printed.
#
# Every compiler in this project is developed against a reference
# implementation -- LLVM for stage08, gforth for stage01, gfortran for
# f77.  This is ISLISP's.  The oracle runs NATIVE code; what is being
# compared is program output.
#
# Usage:   ./oracle.sh prog.lsp
#          ./oracle.sh --check            (0 when the image is present)
# Exit:    the program's own exit status, or 2 when the oracle is missing.
#
# The container mounts the source file's directory as /work and runs
# with that as the working directory, so a program's relative file
# names resolve as they do under the emulator.  Podman on macOS runs in
# a VM that shares home paths but not /tmp, so sources must live under
# $HOME.
set -u

IMAGE="${ISLISP_ORACLE_IMAGE:-slow32:islisp-oracle}"

if command -v podman >/dev/null 2>&1; then ENGINE=podman
elif command -v docker >/dev/null 2>&1; then ENGINE=docker
else echo "oracle.sh: neither podman nor docker found" >&2; exit 2; fi

have_image() {
    "$ENGINE" image exists "$IMAGE" 2>/dev/null || "$ENGINE" images -q "$IMAGE" 2>/dev/null | grep -q .
}

if [ "${1:-}" = "--check" ]; then have_image; exit $?; fi
if [ $# -lt 1 ]; then echo "usage: oracle.sh prog.lsp" >&2; exit 2; fi

SRC="$1"; shift
if [ ! -f "$SRC" ]; then echo "oracle.sh: no such file: $SRC" >&2; exit 2; fi
DIR="$(cd "$(dirname "$SRC")" && pwd)"
BASE="$(basename "$SRC")"

if ! have_image; then
    echo "oracle.sh: image $IMAGE not found." >&2
    echo "  build it:  $ENGINE build -t $IMAGE -f Dockerfile.islisp-oracle ." >&2
    exit 2
fi

exec "$ENGINE" run --rm -i -v "$DIR:/work" -w /work "$IMAGE" "$BASE" "$@"
