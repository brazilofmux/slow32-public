#!/usr/bin/env python3
"""Receive one file over the BBS ZMODEM subset and write it to disk."""
import socket
import sys

ZPAD, ZDLE, ZBIN, ZHEX = 0x2A, 0x18, 0x41, 0x42
ZRQINIT, ZRINIT, ZFILE, ZFIN, ZRPOS, ZDATA, ZEOF = 0, 1, 4, 8, 9, 10, 11
ZCRCE, ZCRCG, ZCRCW = 0x68, 0x69, 0x6B


def crc16(data: bytes) -> int:
    crc = 0
    for b in data:
        crc ^= b << 8
        for _ in range(8):
            if crc & 0x8000:
                crc = ((crc << 1) ^ 0x1021) & 0xFFFF
            else:
                crc = (crc << 1) & 0xFFFF
    return crc


class Link:
    def __init__(self, sock):
        self.s = sock
        self.buf = bytearray()

    def get(self) -> int:
        if not self.buf:
            chunk = self.s.recv(256)
            if not chunk:
                raise EOFError("closed")
            self.buf.extend(chunk)
        return self.buf.pop(0)

    def send(self, data: bytes):
        self.s.sendall(data)


def send_hex(link: Link, typ: int, pos: int = 0):
    raw = bytes([typ, pos & 255, (pos >> 8) & 255, (pos >> 16) & 255, (pos >> 24) & 255])
    c = crc16(raw)
    payload = raw + bytes([(c >> 8) & 255, c & 255])
    hexed = "".join(f"{b:02x}" for b in payload).encode("ascii")
    link.send(bytes([ZPAD, ZPAD, ZDLE, ZHEX]) + hexed + b"\r\n")


def recv_header(link: Link):
    while True:
        c = link.get()
        if c != ZPAD:
            continue
        while c == ZPAD:
            c = link.get()
        if c != ZDLE:
            continue
        kind = link.get()
        if kind in (ZHEX, ZBIN):
            break
    raw = bytearray()
    if kind == ZHEX:
        digits = []
        while len(digits) < 14:
            digits.append(link.get())
        hx = bytes(digits).decode("ascii")
        raw = bytearray(int(hx[i : i + 2], 16) for i in range(0, 14, 2))
        cr = link.get()
        if cr == 0x0D:
            link.get()
    else:
        for _ in range(7):
            c = link.get()
            if c == ZDLE:
                c = link.get() ^ 0x40
            raw.append(c)
    want = crc16(bytes(raw[:5]))
    got = (raw[5] << 8) | raw[6]
    if want != got:
        raise ValueError("crc")
    pos = raw[1] | (raw[2] << 8) | (raw[3] << 16) | (raw[4] << 24)
    return raw[0], pos


def recv_data(link: Link) -> tuple[bytes, int]:
    data = bytearray()
    while True:
        c = link.get()
        if c != ZDLE:
            data.append(c)
            continue
        c = link.get()
        if c in (ZCRCE, ZCRCG, ZCRCW):
            crchi = link.get()
            if crchi == ZDLE:
                crchi = link.get() ^ 0x40
            crclo = link.get()
            if crclo == ZDLE:
                crclo = link.get() ^ 0x40
            crc = crc16(bytes(data) + bytes([c]))
            got = (crchi << 8) | crclo
            if crc != got:
                raise ValueError("data crc")
            return bytes(data), c
        data.append(c ^ 0x40)


def recv_until(link: Link, needle: bytes):
    buf = bytearray()
    while needle not in buf:
        buf.append(link.get())
    return bytes(buf)


def main():
    host, port, name, dest = sys.argv[1], int(sys.argv[2]), sys.argv[3], sys.argv[4]
    s = socket.create_connection((host, port), timeout=10)
    s.settimeout(10)
    link = Link(s)
    recv_until(link, b"Name:")
    link.send(b"alice\r\n")
    recv_until(link, b"Password:")
    link.send(b"secret\r\n")
    recv_until(link, b"Welcome")
    link.send(b"T\r\n")
    recv_until(link, b"File:")
    link.send(name.encode() + b"\r\n")
    recv_until(link, b"ZMODEM")
    typ, _ = recv_header(link)
    if typ != ZRQINIT:
        raise SystemExit(f"expected ZRQINIT got {typ}")
    send_hex(link, ZRINIT, 0)
    typ, _ = recv_header(link)
    if typ != ZFILE:
        raise SystemExit(f"expected ZFILE got {typ}")
    payload, _ = recv_data(link)
    send_hex(link, ZRPOS, 0)
    typ, _ = recv_header(link)
    if typ != ZDATA:
        raise SystemExit(f"expected ZDATA got {typ}")
    out = bytearray()
    while True:
        chunk, end = recv_data(link)
        out.extend(chunk)
        if end == ZCRCE:
            break
    typ, _ = recv_header(link)
    if typ != ZEOF:
        raise SystemExit(f"expected ZEOF got {typ}")
    send_hex(link, ZFIN, 0)
    typ, _ = recv_header(link)
    if typ != ZFIN:
        raise SystemExit(f"expected ZFIN got {typ}")
    open(dest, "wb").write(out)
    zname = payload.split(b"\x00")[0]
    print(f"got {len(out)} bytes name={zname!r}")
    s.close()


if __name__ == "__main__":
    main()
