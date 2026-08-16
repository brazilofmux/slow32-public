#!/bin/bash
# Two guests, one TCP hose, zero shared directories.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
KERMIT_DIR="$(dirname "$SCRIPT_DIR")"
ROOT="$(dirname "$KERMIT_DIR")"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"
KERMIT="$KERMIT_DIR/kermit.s32x"

echo "Building kermit..."
(cd "$KERMIT_DIR" && bash build.sh >/dev/null)

if [ ! -x "$EMU" ]; then
    echo "emulator not found: $EMU"
    exit 1
fi

work="$(mktemp -d "${TMPDIR:-/tmp}/s32-kermit.XXXXXX")"
recv_pid=""
cleanup() {
    if [ -n "$recv_pid" ]; then
        kill "$recv_pid" 2>/dev/null || true
        wait "$recv_pid" 2>/dev/null || true
    fi
    rm -rf "$work"
}
trap cleanup EXIT

fail=0

transfer() { # transfer <label> <file...>  (paths relative to $work/out)
    local label="$1"
    shift
    local inbox="$work/in-$label"
    mkdir -p "$inbox"
    rm -f "$inbox/kermit.port"

    (cd "$inbox" && "$EMU" "$KERMIT" -r) \
        >"$inbox/recv.out" 2>"$inbox/recv.err" &
    recv_pid=$!

    local port=""
    for _ in $(seq 1 50); do
        if [ -f "$inbox/kermit.port" ]; then
            port="$(tr -d '[:space:]' < "$inbox/kermit.port" || true)"
            [ -n "$port" ] && break
        fi
        sleep 0.1
    done
    if [ -z "$port" ]; then
        echo "  FAIL $label: receiver never wrote kermit.port"
        cat "$inbox/recv.out" "$inbox/recv.err" 2>/dev/null || true
        fail=1
        return
    fi

    (cd "$work/out" && "$EMU" "$KERMIT" -s "$port" "$@") \
        >"$work/send.out" 2>"$work/send.err" || true
    wait "$recv_pid" || true
    recv_pid=""

    local f ok=1
    for f in "$@"; do
        local base="${f##*/}"
        if cmp -s "$work/out/$f" "$inbox/$base"; then
            echo "  OK  $label: $base intact ($(wc -c < "$work/out/$f" | tr -d ' ') bytes)"
        else
            echo "  FAIL $label: $base differs or missing"
            ok=0
        fi
    done
    if [ "$ok" -eq 0 ]; then
        echo "--- sender ---";   cat "$work/send.out" "$work/send.err"
        echo "--- receiver ---"; cat "$inbox/recv.out" "$inbox/recv.err"
        fail=1
    fi
}

# The outbox: text, every byte value, a '#'-riddled file, and a big one.
mkdir -p "$work/out"
printf 'From the 1987 desk, with 63 packets of respect.\n' > "$work/out/notes.txt"
python3 - "$work/out/allbytes.bin" <<'PY'
import sys
open(sys.argv[1], 'wb').write(bytes(range(256)) * 16)
PY
printf '###quoted###\x01\x0d\x0a#\x7f###\n' > "$work/out/hashes.txt"
python3 - "$work/out/big.bin" <<'PY'
import sys
open(sys.argv[1], 'wb').write(bytes((i * 7 + (i >> 8)) & 255 for i in range(65536)))
PY

echo "=== Kermit tests ==="
transfer text notes.txt
transfer binary allbytes.bin
transfer quoting hashes.txt
transfer big big.bin
transfer multi notes.txt allbytes.bin hashes.txt

if grep -q "OK (4096 bytes)" "$work/in-binary/recv.out"; then
    echo "  OK  receiver reported 4096 bytes"
else
    echo "  FAIL receiver byte count"
    cat "$work/in-binary/recv.out"
    fail=1
fi

if [ "$fail" -ne 0 ]; then
    echo "=== TESTS FAILED ==="
    exit 1
fi
echo "=== All tests passed ==="
