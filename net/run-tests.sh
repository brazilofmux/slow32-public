#!/bin/bash
# Two-guest echo: server binds 127.0.0.1:0, client connects.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"
DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$DIR"

if [ ! -x "$EMU" ]; then
    echo "emulator not found: $EMU"
    exit 1
fi

if [ ! -f echo_server.s32x ] || [ ! -f echo_client.s32x ]; then
    bash "$DIR/build.sh"
fi

rm -f echo.port
"$EMU" echo_server.s32x >echo_server.out 2>echo_server.err &
server_pid=$!

cleanup() {
    kill "$server_pid" 2>/dev/null || true
    wait "$server_pid" 2>/dev/null || true
}
trap cleanup EXIT

port=""
for _ in $(seq 1 50); do
    if [ -f echo.port ]; then
        port="$(tr -d '[:space:]' < echo.port || true)"
        if [ -n "$port" ]; then
            break
        fi
    fi
    sleep 0.1
done

if [ -z "$port" ]; then
    echo "FAIL: server did not write echo.port"
    echo "--- server stdout ---"
    cat echo_server.out || true
    echo "--- server stderr ---"
    cat echo_server.err || true
    exit 1
fi

"$EMU" echo_client.s32x "$port" >echo_client.out 2>echo_client.err
client_rc=$?
wait "$server_pid"
server_rc=$?
trap - EXIT

if [ "$client_rc" -ne 0 ] || [ "$server_rc" -ne 0 ]; then
    echo "FAIL: client_rc=$client_rc server_rc=$server_rc"
    cat echo_client.out echo_server.out echo_client.err echo_server.err
    exit 1
fi

if ! grep -q 'hello-slow32' echo_client.out; then
    echo "FAIL: client did not echo payload"
    cat echo_client.out
    exit 1
fi

echo "PASS: guest echo 127.0.0.1:$port"
grep 'hello-slow32' echo_client.out
rm -f echo.port
