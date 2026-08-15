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
    while want not in buf:
        chunk = s.recv(256)
        if not chunk:
            break
        buf += chunk
    return buf.decode("ascii", "replace")

out = ""
for step in script.split(";"):
    step = step.strip()
    if step.startswith("WAIT "):
        out = recv_until(step[5:])
    elif step.startswith("SEND "):
        s.sendall((step[5:] + "\r\n").encode())
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

mail="$(dial 'WAIT Name:; SEND alice; WAIT Password:; SEND secret; WAIT Welcome; SEND L; WAIT No messages; SEND P; WAIT To:; SEND all; WAIT Subj:; SEND hello; WAIT Text:; SEND this is a test; WAIT Posted; SEND L; WAIT hello; SEND R; WAIT Read; SEND 1; WAIT this is a test; SEND G; WAIT Goodbye')"
check list-empty "$mail" "No messages"
check posted "$mail" "Posted"
check list-subj "$mail" "hello"
check read-body "$mail" "this is a test"

if [ "$fail" -ne 0 ]; then
    echo "=== server ---"
    cat "$work/server.out" "$work/server.err"
    echo "=== TESTS FAILED ==="
    exit 1
fi
echo "=== All tests passed ==="
