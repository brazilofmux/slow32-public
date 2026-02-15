#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BUCKET="${1:-}"

case "$BUCKET" in
  baseline) MAN="$SCRIPT_DIR/manifests/baseline.lst" ;;
  as-bisect) MAN="$SCRIPT_DIR/manifests/as-bisect.lst" ;;
  ar-bisect) MAN="$SCRIPT_DIR/manifests/ar-bisect.lst" ;;
  *)
    echo "Usage: $0 {baseline|as-bisect|ar-bisect}" >&2
    exit 2
    ;;
esac

cat "$MAN"
