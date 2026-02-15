#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BUCKET="${1:-}"

case "$BUCKET" in
  baseline) MAN="$SCRIPT_DIR/manifests/baseline.lst" ;;
  subset) MAN="$SCRIPT_DIR/manifests/subset.lst" ;;
  subset-idioms) MAN="$SCRIPT_DIR/manifests/subset-stage5-idioms.lst" ;;
  subset-known-gaps) MAN="$SCRIPT_DIR/manifests/subset-known-gaps.lst" ;;
  as-bisect) MAN="$SCRIPT_DIR/manifests/as-bisect.lst" ;;
  ar-bisect) MAN="$SCRIPT_DIR/manifests/ar-bisect.lst" ;;
  *)
    echo "Usage: $0 {baseline|subset|subset-idioms|subset-known-gaps|as-bisect|ar-bisect}" >&2
    exit 2
    ;;
esac

cat "$MAN"
