#!/bin/bash
# Dial the guest BBS from the host. Users live in a real .DBF.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BBS_DIR="$(dirname "$SCRIPT_DIR")"
ROOT="$(dirname "$BBS_DIR")"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"

echo "Building BBS..."
(cd "$BBS_DIR" && bash build.sh)

if [ ! -x "$EMU" ]; then
    echo "emulator not found: $EMU"
    exit 1
fi
if ! command -v python3 >/dev/null 2>&1; then
    echo "python3 required to build USERS.DBF and dial"
    exit 1
fi

work="$(mktemp -d "${TMPDIR:-/tmp}/s32-bbs.XXXXXX")"
cleanup() {
    if [ -n "${server_pid:-}" ]; then
        kill "$server_pid" 2>/dev/null || true
        wait "$server_pid" 2>/dev/null || true
    fi
    rm -rf "$work"
}
trap cleanup EXIT

python3 "$SCRIPT_DIR/make_users.py" "$work/USERS.DBF"

echo "=== Building door.s32x ==="
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
"$LLVM_BIN/clang" -target slow32-unknown-none -S -emit-llvm -O1 \
    -I"$ROOT/runtime/include" "$SCRIPT_DIR/door.c" -o "$work/door.ll"
"$LLVM_BIN/llc" -mtriple=slow32-unknown-none "$work/door.ll" -o "$work/door.s"
"$ROOT/tools/assembler/slow32asm" "$work/door.s" "$work/door.s32o"
"$ROOT/tools/linker/s32-ld" --mmio 64K -o "$work/door.s32x" \
    "$ROOT/runtime/crt0.s32o" "$work/door.s32o" \
    "$ROOT/runtime/libc_mmio.s32a" "$ROOT/runtime/libs32.s32a"

(cd "$work" && "$EMU" "$BBS_DIR/bbs.s32x" USERS.DBF) \
    >"$work/server.out" 2>"$work/server.err" &
server_pid=$!

port=""
for _ in $(seq 1 50); do
    if [ -f "$work/bbs.port" ]; then
        port="$(tr -d '[:space:]' < "$work/bbs.port" || true)"
        if [ -n "$port" ]; then
            break
        fi
    fi
    sleep 0.1
done
if [ -z "$port" ]; then
    echo "FAIL: server did not write bbs.port"
    cat "$work/server.out" "$work/server.err"
    exit 1
fi

dial() {
    local script="$1"
    python3 - "$port" "$script" <<'PY'
import socket, sys
port = int(sys.argv[1])
script = sys.argv[2]
s = socket.create_connection(("127.0.0.1", port), timeout=5)
s.settimeout(5)
buf = b""

def recv_until(needle):
    global buf
    want = needle.encode()
    try:
        while want not in buf:
            chunk = s.recv(256)
            if not chunk:
                break
            buf += chunk
    except socket.timeout:
        sys.stderr.write("dial: TIMEOUT waiting for %r; tail=%r\n"
                         % (needle, buf[-200:]))
    return buf.decode("ascii", "replace")

out = ""
for step in script.split(";"):
    step = step.strip()
    if step.startswith("WAIT "):
        out = recv_until(step[5:])
    elif step == "SEND" or step.startswith("SEND "):
        payload = step[5:] if step.startswith("SEND ") else ""
        s.sendall((payload + "\r\n").encode())
s.close()
sys.stdout.write(out)
PY
}

fail=0
check() {
    local label="$1"
    local got="$2"
    local pat="$3"
    if echo "$got" | grep -q "$pat"; then
        echo "  OK  $label: $pat"
    else
        echo "  FAIL $label: $pat"
        echo "$got"
        fail=1
    fi
}

echo "=== BBS tests  (127.0.0.1:$port) ==="
ok="$(dial 'WAIT Name:; SEND alice; WAIT Password:; SEND secret; WAIT Welcome; SEND W; WAIT only caller; SEND G; WAIT Goodbye')"
check login "$ok" "Welcome, alice"
check who "$ok" "only caller"
check bye "$ok" "Goodbye, alice"

bad="$(dial 'WAIT Name:; SEND alice; WAIT Password:; SEND nope; WAIT Logon')"
check badpass "$bad" "Logon failed"

mail="$(dial 'WAIT Name:; SEND alice; WAIT Password:; SEND secret; WAIT Welcome; SEND L; WAIT No messages; SEND P; WAIT To:; SEND all; WAIT Subj:; SEND hello; WAIT Text; SEND line one; SEND line two; SEND ; WAIT Posted; SEND L; WAIT hello; SEND R; WAIT Read; SEND 1; WAIT line one; SEND G; WAIT Goodbye')"
check list-empty "$mail" "No messages"
check posted "$mail" "Posted"
check list-subj "$mail" "hello"
check read-body "$mail" "line one"
check read-line2 "$mail" "line two"

door="$(dial 'WAIT Name:; SEND alice; WAIT Password:; SEND secret; WAIT Welcome; SEND D; WAIT Door; SEND door; WAIT FORTUNE; WAIT something; SEND hi; WAIT You said; SEND G; WAIT Goodbye')"
check door-banner "$door" "FORTUNE"
check door-echo "$door" "You said: hi"
check door-back "$door" "Goodbye, alice"

# Doors directory: listing, and Rogue as a full-screen door over the socket.
mkdir -p "$work/doors"
cp "$work/door.s32x" "$work/doors/door.s32x"

doorlist="$(dial 'WAIT Name:; SEND alice; WAIT Password:; SEND secret; WAIT Welcome; SEND D; WAIT lists; SEND ?; WAIT Doors:; SEND G; WAIT Goodbye')"
check door-list "$doorlist" "^  door"

if [ -f "$ROOT/rogue/rogue.s32x" ]; then
    cp "$ROOT/rogue/rogue.s32x" "$work/doors/rogue.s32x"
    rogue="$(dial 'WAIT Name:; SEND alice; WAIT Password:; SEND secret; WAIT Welcome; SEND D; WAIT Door; SEND rogue; WAIT Dungeons; SEND Q; WAIT any key; SEND G; WAIT Goodbye')"
    check rogue-door "$rogue" "Dungeons"
    check rogue-quit "$rogue" "\[L\]ist"
else
    echo "  SKIP rogue door (build rogue/ first)"
fi

mkdir -p "$work/files"
printf 'zmodem-payload-ok\n' > "$work/files/payload.bin"
if python3 "$SCRIPT_DIR/zrecv.py" 127.0.0.1 "$port" payload.bin "$work/got.bin" \
        >"$work/zrecv.out" 2>"$work/zrecv.err"; then
    if cmp -s "$work/files/payload.bin" "$work/got.bin"; then
        echo "  OK  zmodem payload"
    else
        echo "  FAIL zmodem payload mismatch"
        echo "  expected: $(od -An -tx1 "$work/files/payload.bin")"
        echo "  got:      $(od -An -tx1 "$work/got.bin" 2>/dev/null || true)"
        fail=1
    fi
else
    echo "  FAIL zmodem transfer"
    cat "$work/zrecv.out" "$work/zrecv.err"
    fail=1
fi

if [ "$fail" -ne 0 ]; then
    echo "=== server ---"
    cat "$work/server.out" "$work/server.err"
    echo "=== TESTS FAILED ==="
    exit 1
fi
echo "=== All tests passed ==="
