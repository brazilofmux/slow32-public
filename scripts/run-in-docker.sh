#!/bin/bash
# Wrapper script to run SLOW-32 programs in Docker using s32run
# Usage: ./scripts/run-in-docker.sh [s32run options] program.s32x [program args...]

# Get the directory where this script lives
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Check if Docker image exists
if ! docker images slow32-emulator | grep -q slow32-emulator; then
    echo "Error: slow32-emulator Docker image not found"
    echo "Build it with: docker build -t slow32-emulator -f Dockerfile.emulator ."
    exit 1
fi

# Process arguments to convert local paths to container paths
declare -a DOCKER_ARGS
for arg in "$@"; do
    # If argument ends with .s32x and exists as a file, convert to /data path
    if [[ "$arg" == *.s32x ]] && [[ -f "$arg" ]]; then
        # Convert to absolute path first, then to container path
        ABS_PATH="$(cd "$(dirname "$arg")" && pwd)/$(basename "$arg")"
        REL_PATH="${ABS_PATH#$PROJECT_ROOT/}"
        DOCKER_ARGS+=("/data/$REL_PATH")
    else
        DOCKER_ARGS+=("$arg")
    fi
done

# Run s32run inside Docker with current directory mounted
docker run --rm -v "$PROJECT_ROOT:/data" slow32-emulator s32run "${DOCKER_ARGS[@]}"
