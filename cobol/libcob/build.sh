#!/bin/bash
# Build the COBOL runtime for SLOW-32.
#
# cobol/ is in the tree's ordinary universe, so the host toolchain builds
# guest code -- the same arrangement fortran/ and every other app use.
# Which host compiler that is, LLVM or the self-hosted stage08 cc, is
# cctool.sh's business.
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
. "$ROOT/cobol/cctool.sh"

s32_cc_obj "$HERE/libcob.s32o" "$HERE/libcob.c" -I"$HERE"
echo "built: $HERE/libcob.s32o ($s32_cc_backend)"
