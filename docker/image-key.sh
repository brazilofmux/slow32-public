#!/usr/bin/env bash
# image-key.sh NAME -- the cache key of an artifact image: a hash of every
# input that can change what it builds (docs/plans/containers.md).  The
# builder tags slow32:NAME-<key>-$ARCH and builds only when ECR lacks it.
#
#   docker/image-key.sh llvm    Dockerfile.llvm
#   docker/image-key.sh flang   Dockerfile.flang
#
# Prints one line, 16 hex digits.  Exit 2 for an unknown NAME; exit 1 if any
# input has uncommitted changes (or untracked files, which would ride into
# the build context), because the key hashes git objects at HEAD and would
# not describe what gets built.  A clean checkout -- what the builder clones
# -- always works.
#
# What a key covers is listed below; the Dockerfile itself is always one of
# the inputs, so its FROM line and its build-arg defaults (LLVM_COMMIT) are
# covered too, which is why the builder never overrides those args.  A missed
# input ships a stale image with green gates: when an image starts reading a
# new file, add it here.  Bump FORMAT when the scheme changes.
set -euo pipefail
FORMAT=1
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
P=llvm-backend/patches
case "${1:-}" in
    llvm)  inputs=(Dockerfile.llvm llvm-backend/SLOW32 llvm-backend/clang
                   $P/01-llvm-triple.patch $P/02-llvm-cmake.patch $P/03-clang-targets.patch
                   $P/04-clang-cmake.patch $P/05-clang-driver.patch) ;;
    # not llvm-backend/SLOW32 or 05: flang writes IR for llc and is run as -fc1
    flang) inputs=(Dockerfile.flang llvm-backend/clang
                   $P/01-llvm-triple.patch $P/02-llvm-cmake.patch $P/03-clang-targets.patch
                   $P/04-clang-cmake.patch $P/07-flang.patch) ;;
    *) echo "usage: image-key.sh llvm|flang" >&2; exit 2 ;;
esac
if [ -n "$(git status --porcelain -- "${inputs[@]}")" ]; then
    echo "image-key.sh: uncommitted changes in the inputs of $1:" >&2
    git status --porcelain -- "${inputs[@]}" >&2
    exit 1
fi
if command -v sha256sum >/dev/null; then sha() { sha256sum; }; else sha() { shasum -a 256; }; fi
{
    echo "format $FORMAT"
    echo "name $1"
    for p in "${inputs[@]}"; do echo "$p $(git rev-parse "HEAD:$p")"; done
} | sha | cut -c1-16
