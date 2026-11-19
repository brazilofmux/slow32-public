#!/usr/bin/env python3
"""The codepage fixtures for test_codepage_tables (docs/plans/xbase-utf8.md,
step 4): three one-field tables of two records -- CPT437.DBF marked CP437
(header byte 29 = 0x01), CPT1252.DBF marked Windows 1252 (0x03), CPTNONE.DBF
with no mark -- each NAME C(8) holding "Müller" in its own codepage's bytes
(none: UTF-8), and a blank record.  Run from xbase/tests/codepage; the files are
committed, this says how they were made."""
import struct

def dbf(path, mark, names):
    fields = [(b'NAME', b'C', 8)]
    hdr_size = 32 + 32 * len(fields) + 1
    rec_size = 1 + sum(f[2] for f in fields)
    h = bytearray(32)
    h[0] = 0x03
    h[1:4] = bytes([126, 11, 19])                 # last update 2026-11-19
    struct.pack_into('<IHH', h, 4, len(names), hdr_size, rec_size)
    h[29] = mark
    out = bytes(h)
    for name, typ, length in fields:
        d = bytearray(32)
        d[0:len(name)] = name
        d[11] = typ[0]
        d[16] = length
        out += bytes(d)
    out += b'\r'
    for n in names:
        out += b' ' + n.ljust(8, b' ')[:8]
    out += b'\x1a'
    open(path, 'wb').write(out)

dbf('CPT437.DBF', 0x01, ['Müller'.encode('cp437'), b''])
dbf('CPT1252.DBF', 0x03, ['Müller'.encode('cp1252'), b''])
dbf('CPTNONE.DBF', 0x00, ['Müller'.encode('utf-8'), b''])
