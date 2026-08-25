#!/bin/bash
# Wrapper script to run SLOW-32 programs in a container using s32run.
# Usage: ./scripts/run-in-docker.sh [s32run options] program.s32x [program args...]
#
# Uses podman when available, docker otherwise.  The image is
# slow32:emulator (repo:tag naming, matching ~/builder's jobs and the
# ECR mirror); the old hyphenated slow32-emulator name is accepted as
# a fallback for stale local builds.

# Get the directory where this script lives
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Container engine: podman preferred, docker fallback
if command -v podman >/dev/null 2>&1; then
    ENGINE=podman
elif command -v docker >/dev/null 2>&1; then
    ENGINE=docker
else
    echo "Error: neither podman nor docker found" >&2
    exit 1
fi

# Image: slow32:emulator, with the legacy hyphenated name as fallback
IMAGE=slow32:emulator
if ! "$ENGINE" image exists "$IMAGE" 2>/dev/null && \
   ! "$ENGINE" images -q "$IMAGE" 2>/dev/null | grep -q .; then
    if "$ENGINE" images -q slow32-emulator 2>/dev/null | grep -q .; then
        IMAGE=slow32-emulator
    else
        echo "Error: slow32:emulator image not found" >&2
        echo "Build it with: $ENGINE build -t slow32:emulator -f Dockerfile.emulator ." >&2
        exit 1
    fi
fi

# Process arguments to convert local paths to container paths
declare -a RUN_ARGS
for arg in "$@"; do
    # If argument ends with .s32x and exists as a file, convert to /data path
    if [[ "$arg" == *.s32x ]] && [[ -f "$arg" ]]; then
        # Convert to absolute path first, then to container path
        ABS_PATH="$(cd "$(dirname "$arg")" && pwd)/$(basename "$arg")"
        REL_PATH="${ABS_PATH#$PROJECT_ROOT/}"
        RUN_ARGS+=("/data/$REL_PATH")
    else
        RUN_ARGS+=("$arg")
    fi
done

# Run s32run inside the container with the project root mounted
"$ENGINE" run --rm -v "$PROJECT_ROOT:/data" "$IMAGE" s32run "${RUN_ARGS[@]}"
