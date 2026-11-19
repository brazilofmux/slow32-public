#!/usr/bin/env python3
"""Connect as a tube viewer and send 300 KEYE events in one write: more
than the 256 the queue holds.  Codes 0x200+i number them; event 100,
which survives, carries 0xFE in its reserved byte, which the host must
not pass on."""
import os
import socket
import struct
import sys
import time

if len(sys.argv) < 2:
    sys.stderr.write("usage: inject.py PORTFILE\n")
    sys.exit(2)

port_file = sys.argv[1]
port = 0
for _ in range(250):
    if os.path.exists(port_file):
        try:
            port = int(open(port_file).read().strip())
            if 0 < port < 65536:
                break
        except (OSError, ValueError):
            pass
    time.sleep(0.02)
else:
    sys.stderr.write("no port file\n")
    sys.exit(1)

s = socket.create_connection(("127.0.0.1", port))
s.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)


def recvn(n):
    buf = b""
    while len(buf) < n:
        chunk = s.recv(n - len(buf))
        if not chunk:
            raise EOFError
        buf += chunk
    return buf


hdr = recvn(8)
length, _tag = struct.unpack("<II", hdr)
if length > 4:
    recvn(length - 4)


def keye(code, down, reserved=0):
    return struct.pack("<IIHBB", 8, 0x4559454B, code, down, reserved)


s.sendall(b"".join(keye(0x200 + i, i & 1, 0xFE if i == 100 else 0) for i in range(300)))

try:
    while True:
        hdr = recvn(8)
        length, _tag = struct.unpack("<II", hdr)
        if length > 4:
            recvn(length - 4)
except (OSError, EOFError):
    pass
