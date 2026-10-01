#!/usr/bin/env python3
"""STRING and UNSTRING as X3.23-1985 describes them (VI-131 to VI-133,
STRING general rules 1-10; VI-137 to VI-139, UNSTRING general rules
1-20), written out independently of either compiler: a reference oracle
for tests/gen (gen-string.py).  All items are alphanumeric, as the
generator writes them; receiving moves follow the alphanumeric MOVE
(left-justified, space-filled, truncated on the right).
"""


def move_x(value, size):
    return (value + " " * size)[:size]


def string(sources, receiver, pointer):
    """sources: [(content, delimiter or None for SIZE)];
    returns (receiver, pointer, overflow)"""
    out = list(receiver)
    for content, delim in sources:
        if delim is not None:
            at = content.find(delim)            # rule 4b: up to the delimiter, not including it
            content = content[:at] if at >= 0 else content
        for ch in content:
            # rule 9: before each character's move, a pointer outside the
            # receiver ends the statement in the overflow condition
            if pointer < 1 or pointer > len(out):
                return "".join(out), pointer, True
            out[pointer - 1] = ch               # rule 7: one at a time, then the pointer up by one
            pointer += 1
    return "".join(out), pointer, False          # rule 8: positions not reached keep their data


def unstring(send, delims, receivers, pointer, tally):
    """delims: [(text, all?)] in the order written; receivers: one dict per
    INTO phrase, {"size": n, "value": s, "dsize": n or None, "dval": s,
    "count": None or value}.  Returns (receivers, pointer, tally, overflow).
    Values of items not acted on are returned unchanged."""
    rs = [dict(r) for r in receivers]
    n = len(send)
    # rule 17a: an overflow at initiation, nothing changed
    if pointer < 1 or pointer > n:
        return rs, pointer, tally, True
    pos = pointer - 1                            # rule 13a
    i = 0
    while True:
        # rule 13b: left to right to the first delimiter; at each position
        # the delimiters are tried in the order written (rule 12)
        start = pos
        found = None
        while pos < n and found is None:
            for d, all_ in delims:
                if send.startswith(d, pos):
                    found = (d, all_)
                    break
            if found is None:
                pos += 1
        field = send[start:pos]
        r = rs[i]
        r["value"] = move_x(field, r["size"])    # rule 13c (and rule 9: empty -> spaces)
        if found:
            d, all_ = found
            pos += len(d)
            if all_:                             # rule 8: contiguous occurrences are one
                while send.startswith(d, pos):
                    pos += len(d)
            if r.get("dsize"):
                r["dval"] = move_x(d, r["dsize"])  # rule 13d: one occurrence
        elif r.get("dsize"):
            r["dval"] = " " * r["dsize"]        # 13d: the end of the sending item
        if r.get("count") is not None:
            r["count"] = len(field)             # rule 13e
        tally += 1                               # rule 16
        i += 1
        # rule 13g: until the sending item is exhausted or no receiver is left
        if pos >= n:
            return rs, pos + 1, tally, False     # rule 15: the pointer counts every character examined
        if i == len(rs):
            return rs, pos + 1, tally, True      # rule 17b: characters left unexamined
