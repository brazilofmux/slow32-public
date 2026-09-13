#!/bin/bash
# Build slow32:kit -- slow32:base plus the kit (~/s32x) -- and, with --push,
# build it for BOTH architectures and push the arch tags to ECR for
# ~/builder/manifest.sh to stitch.
#
# Why this is not a ~/builder job: the kit is a hand-curated drop of build
# products (see selfhost/stage08/RUNTIME_KIT.md "Regenerating" and the
# provenance map), not a git repository, so a clean clone has nothing to
# build it from.  This script runs on a machine that has the kit.  The
# Dockerfile is COPY-only, so the non-native architecture builds without
# emulation on top of that arch's base pulled from ECR.
#
#   scripts/build-kit-image.sh            local: host arch, FROM slow32:base
#   scripts/build-kit-image.sh --push     both arches FROM ECR base, gated, pushed
#
# Gate (always, host arch): regression/run-kit-differential.sh and
# run-kit-tools-differential.sh INSIDE the built image, i.e. the kit's own
# tools driven on the image's engines.
#
# EXCLUDED from the image (the same editorial line as the public mirror,
# applied to layers -- an image is an extractable archive):
#   regal.s32x   the user's ledger tooling; not for public consumption
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
KIT="${KIT:-$HOME/s32x}"
REP1="274462252673.dkr.ecr.us-west-2.amazonaws.com"
EXCLUDE=(regal.s32x .DS_Store)
PUSH=0
[ "${1:-}" = "--push" ] && PUSH=1

[ -f "$KIT/cc.s32x" ] || { echo "no kit at $KIT (set KIT=)"; exit 1; }
case "$(uname -m)" in x86_64) HOST=amd64 ;; arm64|aarch64) HOST=arm64 ;; *) echo "arch?"; exit 1 ;; esac

# Stage the context under $HOME (podman's VM does not share /tmp).
CTX="$(mktemp -d "$HOME/.kit-image.XXXXXX")"
trap 'rm -rf "$CTX"' EXIT
mkdir -p "$CTX/kit" "$CTX/bin"
ex=(); for e in "${EXCLUDE[@]}"; do ex+=(--exclude "$e"); done
rsync -a "${ex[@]}" "$KIT/" "$CTX/kit/"
cp "$ROOT/docker/bin/s32kcc" "$CTX/bin/s32kcc"; chmod +x "$CTX/bin/s32kcc"
for e in "${EXCLUDE[@]}"; do [ ! -e "$CTX/kit/$e" ] || { echo "exclusion failed: $e"; exit 1; }; done
echo "context: $(du -sh "$CTX/kit" | cut -f1) from $KIT, excluding: ${EXCLUDE[*]}"

build() {  # build <arch> <base-image> <tag>
    podman build --platform "linux/$1" -f "$ROOT/Dockerfile.kit" \
        --build-arg BASE_IMAGE="$2" -t "$3" "$CTX" >/dev/null
    echo "built $3 ($1)"
}

if [ "$PUSH" = 1 ]; then
    aws ecr get-login-password --region us-west-2 | podman login --username AWS --password-stdin "$REP1" >/dev/null
    podman pull -q --platform "linux/$HOST" "$REP1/slow32:base" >/dev/null
    build "$HOST" "$REP1/slow32:base" "slow32:kit-$HOST"
    HOST_TAG="slow32:kit-$HOST"
else
    build "$HOST" "slow32:base" "slow32:kit"
    HOST_TAG="slow32:kit"
fi

echo "--- gate: kit differentials inside $HOST_TAG"
podman run --rm -v "$ROOT:/ws" -w /ws/regression \
    -e KIT=/opt/slow32/kit \
    -e SLOW32=/usr/local/bin/slow32 -e SLOW32_FAST=/usr/local/bin/slow32-fast \
    -e SLOW32_DBT=/usr/local/bin/slow32-dbt -e QEMU_S32=/usr/local/bin/qemu-system-slow32 \
    --entrypoint bash "$HOST_TAG" -c './run-kit-differential.sh && ./run-kit-tools-differential.sh'

if [ "$PUSH" = 1 ]; then
    for arch in arm64 amd64; do
        [ "$arch" = "$HOST" ] || {
            podman pull -q --platform "linux/$arch" "$REP1/slow32:base" >/dev/null
            build "$arch" "$REP1/slow32:base" "slow32:kit-$arch"
        }
        podman tag "slow32:kit-$arch" "$REP1/slow32:kit-$arch"
        podman push -q "$REP1/slow32:kit-$arch"
        echo "pushed $REP1/slow32:kit-$arch"
    done
    echo "now: ~/builder/manifest.sh stitches slow32:kit"
fi
