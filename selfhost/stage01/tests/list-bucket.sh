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
  reloc-bisect) MAN="$SCRIPT_DIR/manifests/reloc-bisect.lst" ;;
  reloc-bisect-lo12) MAN="$SCRIPT_DIR/manifests/reloc-bisect-lo12.lst" ;;
  reloc-bisect-lo12-pack) MAN="$SCRIPT_DIR/manifests/reloc-bisect-lo12-pack.lst" ;;
  reloc-bisect-lo12-rdwr) MAN="$SCRIPT_DIR/manifests/reloc-bisect-lo12-rdwr.lst" ;;
  *)
    echo "Usage: $0 {baseline|subset|subset-idioms|subset-known-gaps|as-bisect|ar-bisect|reloc-bisect|reloc-bisect-lo12|reloc-bisect-lo12-pack|reloc-bisect-lo12-rdwr}" >&2
    exit 2
    ;;
esac

cat "$MAN"
