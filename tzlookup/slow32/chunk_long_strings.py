#!/usr/bin/env python3
"""Rewrite over-long .ascii/.asciz lines into wrapped .byte directives.

The SLOW-32 assembler caps a token at MAX_TOKEN_LEN (16 KB). LLVM emits a large
char array (e.g. TZ_ZIP_ZONE[100000]) as a single .ascii string far past that.
This filter decodes such a string back to bytes and re-emits it as .byte lines,
which the assembler handles fine. Short lines pass through untouched.

Usage: chunk_long_strings.py in.s out.s
"""
import sys

THRESHOLD = 8192   # rewrite string lines longer than this
PER_LINE = 32      # bytes per emitted .byte directive


def decode(s):
    """Decode a C/asm string literal body (no surrounding quotes) to bytes."""
    out = bytearray()
    i, n = 0, len(s)
    simple = {'n': 10, 't': 9, 'r': 13, 'f': 12, 'b': 8,
              'a': 7, 'v': 11, '0': 0, '"': 34, '\\': 92}
    while i < n:
        c = s[i]
        if c != '\\':
            out.append(ord(c))
            i += 1
            continue
        nxt = s[i + 1]
        if nxt in 'x':                       # \xHH
            out.append(int(s[i + 2:i + 4], 16))
            i += 4
        elif nxt.isdigit():                  # \o, \oo, or \ooo (octal)
            j = i + 1
            k = j
            while k < n and k - j < 3 and s[k] in '01234567':
                k += 1
            out.append(int(s[j:k], 8) & 0xFF)
            i = k
        else:                                # \n, \t, \\, \" ...
            out.append(simple.get(nxt, ord(nxt)))
            i += 2
    return out


def emit_bytes(data, indent):
    lines = []
    for off in range(0, len(data), PER_LINE):
        chunk = data[off:off + PER_LINE]
        lines.append(indent + ".byte " + ", ".join(str(b) for b in chunk))
    return lines


def main():
    with open(sys.argv[1]) as f:
        src = f.readlines()
    out = []
    for line in src:
        stripped = line.rstrip("\n")
        body = stripped.lstrip()
        if len(stripped) > THRESHOLD and (
                body.startswith(".ascii") or body.startswith(".asciz")):
            indent = stripped[:len(stripped) - len(body)]
            q0 = stripped.index('"')
            q1 = stripped.rindex('"')
            data = decode(stripped[q0 + 1:q1])
            if body.startswith(".asciz"):
                data.append(0)               # .asciz adds a trailing NUL
            out.extend(l + "\n" for l in emit_bytes(data, indent))
        else:
            out.append(line)
    with open(sys.argv[2], "w") as f:
        f.writelines(out)


if __name__ == "__main__":
    main()
