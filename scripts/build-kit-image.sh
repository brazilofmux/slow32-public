#!/bin/bash
# Build slow32:selfhost and slow32:kit from the tree for this machine's
# architecture, and gate the kit inside the image.  ~/builder builds the
# same two Dockerfiles in a round, per arch, and pushes them; this is the
# local check (docs/plans/containers.md).
#
#   scripts/build-kit-image.sh [--base IMG] [--toolchain IMG]
#
# --base       the slow32:base to build on (default slow32:base)
# --toolchain  the image LLVM comes from (default slow32:toolchain)
#
# Gate, inside the kit image: regression/run-kit-differential.sh,
# run-kit-tools-differential.sh and run-kit-islisp.sh -- the kit's own
# tools driven on the image's engines, and the shipped islisp.s32x over
# islisp/tests -- and run-kit-basic.sh, the kit's BASIC compilers against its
# interpreter.  (BUILD_TIMEOUT: the harness's 180 s skips a test whose build
# takes longer, with exit 0, and the largest take that on a 4-vCPU host.)
#
# The kit was a hand-made drop (~/s32x) until 2026-11-08; now ~/s32x is
# made from the tree (scripts/build-kit.sh ~/s32x), the same bytes the
# image holds.  regal.s32x is never in either.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BASE=slow32:base; TOOLCHAIN=slow32:toolchain
while [ $# -gt 0 ]; do
    case "$1" in
        --base) BASE="$2"; shift ;;
        --toolchain) TOOLCHAIN="$2"; shift ;;
        *) echo "usage: build-kit-image.sh [--base IMG] [--toolchain IMG]" >&2; exit 2 ;;
    esac
    shift
done
cd "$ROOT"
podman build -q -f Dockerfile.selfhost --build-arg BASE_IMAGE="$BASE" -t slow32:selfhost . >/dev/null
echo "built slow32:selfhost"
podman build -q -f Dockerfile.kit --build-arg BASE_IMAGE="$BASE" --build-arg TOOLCHAIN_IMAGE="$TOOLCHAIN" \
    --build-arg SELFHOST_IMAGE=slow32:selfhost -t slow32:kit . >/dev/null
echo "built slow32:kit"
if podman run --rm slow32:kit test -e /opt/slow32/kit/regal.s32x; then
    echo "regal.s32x is in the kit" >&2; exit 1
fi

echo "--- gate: kit differentials, kit islisp and kit BASIC inside slow32:kit"
podman run --rm -v "$ROOT:/ws" -w /ws/regression \
    -e KIT=/opt/slow32/kit -e BUILD_TIMEOUT="${BUILD_TIMEOUT:-1800}" \
    -e SLOW32=/usr/local/bin/slow32 -e SLOW32_FAST=/usr/local/bin/slow32-fast \
    -e SLOW32_DBT=/usr/local/bin/slow32-dbt -e QEMU_S32=/usr/local/bin/qemu-system-slow32 \
    --entrypoint bash slow32:kit -c './run-kit-differential.sh && ./run-kit-tools-differential.sh && ./run-kit-islisp.sh && ./run-kit-basic.sh'
