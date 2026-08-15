#!/usr/bin/env python3
"""Write a dBase III USERS.DBF with NAME and PASS fields."""
import struct
import sys


def rec(name, typ, length):
    buf = bytearray(32)
    raw = name.encode("ascii")[:10]
    buf[0 : len(raw)] = raw
    buf[11] = ord(typ)
    buf[16] = length
    return bytes(buf)


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else "USERS.DBF"
    fields = [rec("NAME", "C", 16), rec("PASS", "C", 16)]
    users = [("alice", "secret"), ("bob", "hunter2")]
    rec_size = 1 + 16 + 16
    hdr_size = 32 + 32 * len(fields) + 1
    hdr = bytearray(32)
    hdr[0] = 0x03
    hdr[1], hdr[2], hdr[3] = 126, 8, 15
    hdr[4:8] = struct.pack("<I", len(users))
    hdr[8:10] = struct.pack("<H", hdr_size)
    hdr[10:12] = struct.pack("<H", rec_size)
    with open(path, "wb") as f:
        f.write(hdr)
        for field in fields:
            f.write(field)
        f.write(b"\x0d")
        for name, pw in users:
            row = bytearray(rec_size)
            row[0] = ord(" ")
            row[1:17] = name.encode("ascii").ljust(16)
            row[17:33] = pw.encode("ascii").ljust(16)
            f.write(row)
        f.write(b"\x1a")


if __name__ == "__main__":
    main()
