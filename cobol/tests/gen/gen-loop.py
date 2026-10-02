#!/usr/bin/env python3
"""Generate a random program of in-line loops whose items are binary
integers (GEN=loop tests/gen/run-flag.sh -fno-loop-reg ..., and GEN=loop
tests/gen/run-self.sh REV ...).

    gen-loop.py SEED [LOOPS] > prog.cbl

Inside an in-line PERFORM's loop the compiler keeps a binary item in a
register as well as in storage (src/cobc/loopreg.h; docs/performance.md)
-- when nothing in the loop can change the item behind the register.
What "nothing" has to cover is the whole of this generator: loops of
every form (VARYING, with AFTER, UNTIL, TIMES, TEST AFTER), nested, whose
bodies read their items, step them, and also change them in every way
that is not a plain store to the item:

- through a REDEFINES of it, as characters; through the group it is in,
  by a group MOVE, INITIALIZE, or a reference-modified part of the group;
- as an element of a table laid over it, by a subscript computed while
  running;
- in a paragraph performed out of line from the body, and in a paragraph
  left to with GO TO from an out-of-line loop;
- by READ ... INTO the group; as a file's record area, by READ; as the
  FILE STATUS item of a file written in the loop;
- as a receiver the runtime stores: COMPUTE ROUNDED, STRING's POINTER,
  INSPECT TALLYING, DIVIDE REMAINDER, COMPUTE with SIZE ERROR;
- and by plain stores, which the register must follow: MOVE, ADD,
  SUBTRACT, COMPUTE, with and without truncation by the picture.

Between the loops, and inside them, straight runs of the same statements
with IFs whose two sides do different things to an item: the compiler
also takes a load from a register when the item was loaded or stored
earlier on every way to it and nothing since could have changed it.

Items are COMP of two and four bytes, signed and not, COMP-5, one byte,
and unsigned DISPLAY integers (kept by their digits), alone at level 01
and inside groups beside other items.  Every loop
counts its passes in a DISPLAY item and stops at a limit, so every
program ends whatever the body did to the loop's own item; each loop
prints its items when it is over, and now and then inside.

The same compiler with the rewrite off (-fno-loop-reg; -fno-avail-reg
for the second kind alone) is the oracle: the two programs must print
the same bytes (run-flag.sh).
"""
import random
import sys

KINDS = [
    ("pic 9(4) comp", 9999), ("pic s9(4) comp", 9999), ("pic 9(9) comp", 99999), ("pic s9(9) comp", 99999),
    ("pic 9(4) comp-5", 9999), ("pic s9(4) comp-5", 9999), ("pic 9(9) comp-5", 99999),
    ("pic 9(2) comp", 99), ("pic 9(3) comp", 999), ("pic s9(8) comp", 99999),
    ("pic 9(4)", 9999), ("pic 99", 99), ("pic 9(5)", 99999), ("pic 9", 9),
]
FUEL = 60


def main():
    seed = int(sys.argv[1])
    nloops = int(sys.argv[2]) if len(sys.argv) > 2 else 12
    r = random.Random(seed * 2654435761 % 2**31 + 7)
    out = []
    w = out.append

    # the items: A0.. alone; G-items in a group with a REDEFINES over the
    # group and one over a single item; a table over the group
    alone = [("A%d" % i,) + r.choice(KINDS) for i in range(5)]
    w("identification division.")
    w("program-id. genloop.")
    w("environment division.")
    w("input-output section.")
    w("file-control.")
    w('    select f1 assign to "genloop1.dat" organization sequential file status is st.')
    w('    select f2 assign to "genloop2.dat" organization sequential file status is fst.')
    w("data division.")
    w("file section.")
    w("fd  f1.")
    w("01  f1-rec.")
    w("    05  f1-n     pic 9(4) comp.")
    w("    05  f1-x     pic x(6).")
    w("fd  f2.")
    w("01  f2-rec       pic x(8).")
    w("working-storage section.")
    for name, pic, _ in alone:
        w("01  %s %s value 0." % (name, pic))
    # the group: four two-byte COMP items and one four-byte, 12 bytes
    w("01  G.")
    w("    05  G1 pic 9(4) comp value 0.")
    w("    05  G2 pic s9(4) comp value 0.")
    w("    05  G3 pic 9(4) comp value 0.")
    w("    05  G4 pic 9(9) comp value 0.")
    w("    05  G5 pic 9(4) comp value 0.")
    w("01  GX redefines G.")
    w("    05  GX1 pic xx.")
    w("    05  GT pic 9(4) comp occurs 2.")
    w("    05  filler pic x(6).")
    # DISPLAY integers in a group, and the group as characters
    w("01  DG.")
    w("    05  D1 pic 99 value 0.")
    w("    05  D2 pic 9(4) value 0.")
    w("01  DX redefines DG pic x(6).")
    w("01  GZ.")
    w("    05  filler pic 9(4) comp value 3.")
    w("    05  filler pic s9(4) comp value 2.")
    w("    05  filler pic 9(4) comp value 4.")
    w("    05  filler pic 9(9) comp value 1.")
    w("    05  filler pic 9(4) comp value 5.")
    # a status item that is also a number: two bytes, redefined
    w("01  ST-AREA.")
    w("    05  st pic xx.")
    w("01  ST-N redefines ST-AREA pic 9(4) comp.")
    w("01  fst pic xx.")
    w("01  FUEL pic 9(5) value 0.")
    w("01  SUM1 pic s9(9) comp value 0.")
    w("01  SUM2 pic 9(9) value 0.")
    w("01  K pic 9(4) comp value 1.")
    w("01  TXT pic x(12) value \"0123456789AB\".")
    w("01  C1 pic x.")
    w("01  PTR pic 9(4) comp value 1.")
    w("01  REM1 pic 9(4) comp.")
    w("01  TAL pic 9(4) comp value 0.")
    w("01  DSP pic 9(4) value 0.")

    ints = [a[0] for a in alone] + ["G1", "G2", "G3", "G4", "G5", "K", "PTR", "TAL", "ST-N", "D1", "D2"]
    caps = dict((a[0], a[2]) for a in alone)
    caps.update({"G1": 9999, "G2": 9999, "G3": 9999, "G4": 99999, "G5": 9999, "K": 9999, "PTR": 9999, "TAL": 9999, "ST-N": 9999,
                 "D1": 99, "D2": 9999})

    w("procedure division.")
    w("main-para.")
    w("    open output f1")
    w("    perform varying K from 1 by 1 until K > 9")
    w("        move K to f1-n  move \"record\" to f1-x  write f1-rec")
    w("    end-perform")
    w("    close f1")
    w("    open input f1")
    w("    open output f2")
    w("    move 1 to K")

    paras = []          # out-of-line paragraphs the bodies perform

    def val(lim=9):
        return r.randint(0, lim)

    def reader(x, ind):
        """a statement that reads item x"""
        c = r.randrange(6)
        if c == 0:
            return "%sadd %s to SUM1" % (ind, x)
        if c == 1:
            return "%scompute SUM2 = function mod(SUM2 * 3 + %s, 99991)" % (ind, x)
        if c == 2:
            return "%sif %s > %d add 1 to SUM1 else add 2 to SUM1 end-if" % (ind, x, val(20))
        if c == 3:
            return "%smove TXT(function mod(%s, 12) + 1:1) to C1  if C1 > \"5\" add 1 to SUM2 end-if" % (ind, x)
        if c == 4:
            return "%sif function mod(FUEL, 13) = 5 display \"  in: \" %s end-if" % (ind, x)
        return "%scompute SUM1 = SUM1 + %s * 2 - 1" % (ind, x)

    def writer(x, ind):
        """a statement that changes item x: plain stores and everything else"""
        c = r.randrange(30)
        v = val(min(caps.get(x, 99), 40))
        if c < 3:
            return "%sadd %d to %s" % (ind, r.randint(1, 3), x)
        if c == 3:
            return "%smove %d to %s" % (ind, v, x)
        if c == 4:
            return "%scompute %s = %s + %d" % (ind, x, x, r.randint(1, 4))
        if c == 5:
            return "%ssubtract 1 from %s" % (ind, x) if caps.get(x, 0) and "G2" == x else "%sadd 1 to %s" % (ind, x)
        if c == 6:      # the group, whole
            return "%sif function mod(FUEL, 7) = 3 move GZ to G end-if" % ind
        if c == 7:
            return "%sif function mod(FUEL, 11) = 4 initialize G end-if" % ind
        if c == 8:      # a part of the group: G1's low byte, G2's high byte
            return "%sif function mod(FUEL, 5) = 2 move x\"01\" to G(%d:1) end-if" % (ind, r.choice([2, 2, 3, 6]))
        if c == 9:      # the redefinition
            return "%sif function mod(FUEL, 6) = 1 move x\"0002\" to GX1 end-if" % ind
        if c == 10:     # an element of the table over G2 and G3, the subscript an item
            return "%scompute K = function mod(FUEL, 2) + 1  move %d to GT(K)" % (ind, v)
        if c == 11:     # a paragraph performed from the body
            p = "bump-%d" % len(paras)
            paras.append((p, r.choice(ints), r.randint(1, 3)))
            return "%sperform %s" % (ind, p)
        if c == 12:
            return "%sread f1 into G at end move 1 to G1 end-read" % ind
        if c == 13:     # the record area is an item too; READ changes it
            return "%sread f1 at end move 0 to f1-n end-read  add f1-n to SUM1" % ind
        if c == 14:     # the status item, which ST-N is also
            return "%smove \"genloop!\" to f2-rec  write f2-rec  add 1 to SUM2" % ind
        if c == 15:
            return "%smove 1 to PTR  string \"ab\" delimited by size into TXT with pointer PTR  move \"0123456789AB\" to TXT" % ind
        if c == 16:
            return "%scompute %s rounded = (%d + FUEL) / 2" % (ind, x, v)       # the runtime's store
        if c == 17:
            return "%smove 0 to TAL  inspect TXT tallying TAL for all \"%s\"" % (ind, r.choice("0123A"))
        if c == 18:
            return "%sdivide FUEL by 7 giving DSP remainder REM1  add REM1 to SUM1" % ind
        if c == 19:
            return "%scompute %s = %s + 1 on size error move 1 to %s end-compute" % (ind, x, x, x)
        if c == 20:     # a DISPLAY item beside: not kept in a register, moved to one that is
            return "%smove FUEL to DSP  move DSP to %s" % (ind, x) if caps.get(x, 0) >= 9999 else "%sadd 1 to %s" % (ind, x)
        if c == 21:
            return "%smove G3 to %s" % (ind, x)
        if c == 22:
            return "%smultiply 2 by %s" % (ind, x) if caps.get(x, 0) >= 9999 else "%sadd 1 to %s" % (ind, x)
        if c == 23:
            return "%sif st = \"00\" add 1 to SUM2 else add 3 to SUM2 end-if" % ind
        if c == 24:
            return "%sif ST-N > 100 add 1 to SUM1 end-if" % ind
        if c == 26:     # the DISPLAY group, as characters
            return "%sif function mod(FUEL, 4) = 1 move \"%02d%04d\" to DX end-if" % (ind, val(20), val(40))
        if c == 27:
            return "%smove \"%d\" to DX(%d:1)" % (ind, val(9), r.choice([1, 2, 3, 6]))
        if c == 28:
            return "%sif function mod(FUEL, 9) = 2 move zeros to DG end-if" % ind
        return "%sadd 1 to %s %s" % (ind, x, r.choice(ints))

    def run(ind, items=None):
        """a straight run: readers and writers of a few items, and IFs whose sides differ"""
        lines = []
        xs = items or r.sample(ints, 2)
        for _ in range(r.randint(3, 8)):
            x = r.choice(xs) if r.random() < 0.7 else r.choice(ints)
            c = r.random()
            if c < 0.45:
                lines.append(reader(x, ind))
            elif c < 0.7:
                lines.append(writer(x, ind))
            elif c < 0.9:
                cond = r.choice(["function mod(FUEL, 3) = 1", "%s > %d" % (x, val(9)), "SUM2 > 500"])
                a = (writer if r.random() < 0.6 else reader)(x, ind + "    ")
                b = (writer if r.random() < 0.4 else reader)(r.choice(xs), ind + "    ")
                lines.append("%sif %s" % (ind, cond)); lines.append(a)
                if r.random() < 0.7:
                    lines.append("%selse" % ind); lines.append(b)
                lines.append("%send-if" % ind)
                lines.append(reader(x, ind))
            else:
                lines.append("%sadd 1 to FUEL" % ind)
        return lines

    def body(items, depth, ind):
        """a loop's statements: the pass counted, then readers and writers of its items and of others"""
        lines = ["%sadd 1 to FUEL" % ind]
        if r.random() < 0.3:
            lines += run(ind, items)
        for _ in range(r.randint(1, 5)):
            x = r.choice(items) if r.random() < 0.6 else r.choice(ints)
            c = r.random()
            if c < 0.5:
                lines.append(reader(x, ind))
            elif c < 0.85:
                lines.append(writer(x, ind))
            elif c < 0.92 and depth < 2:
                lines.extend(loop(depth + 1, ind))
            elif c < 0.96:
                lines.append("%sif function mod(FUEL, %d) = 0 exit perform cycle end-if" % (ind, r.randint(3, 9)))
            else:
                lines.append("%sif FUEL > %d exit perform end-if" % (ind, r.randint(FUEL // 2, FUEL)))
        return lines

    def loop(depth, ind):
        """one in-line loop, as lines"""
        form = r.randrange(7)
        x = r.choice(ints); y = r.choice([i for i in ints if i != x])
        lim = r.randint(2, 8)
        stop = "FUEL > %d" % (FUEL * (depth + 1))
        test = r.choice(["", "", "", "with test after "])
        lines = []
        inner = ind + "    "
        if form < 3:
            lines.append("%sperform %svarying %s from %d by %d until %s > %d or %s" % (ind, test, x, r.randint(0, 2), r.randint(1, 2), x, lim, stop))
            lines += body([x, y], depth, inner)
        elif form == 3:
            lines.append("%sperform %svarying %s from 1 by 1 until %s > %d or %s" % (ind, test, x, x, r.randint(2, 4), stop))
            lines.append("%s    after %s from 1 by 1 until %s > %d or %s" % (ind, y, y, r.randint(2, 3), stop))
            lines += body([x, y], depth, inner)
        elif form == 4:
            lines.append("%smove %d to %s" % (ind, r.randint(0, 2), x))
            lines.append("%sperform %suntil %s > %d or %s" % (ind, test, x, lim, stop))
            lines += body([x, y], depth, inner)
            lines.append("%sadd 1 to %s" % (inner, x))
        elif form == 5:
            lines.append("%smove %d to %s" % (ind, r.randint(2, 6), y))
            lines.append("%sperform %s times" % (ind, r.choice([str(r.randint(2, 6)), y])))
            lines += body([x, y], depth, inner)
        else:           # compared with another item, which the body may change
            lines.append("%smove %d to %s" % (ind, lim, y))
            lines.append("%sperform varying %s from 1 by 1 until %s > %s or %s" % (ind, x, x, y, stop))
            lines += body([x, y], depth, inner)
        lines.append("%send-perform" % ind)
        return lines

    for n in range(nloops):
        w("    move 0 to FUEL")
        for line in run("    "):
            w(line)
        for line in loop(0, "    "):
            w(line)
        if r.random() < 0.5:
            for line in run("    "):
                w(line)
        w('    display "%d: " %s' % (n, " \" \" ".join(r.sample(ints, 5))))
        w('    display "   " SUM1 " " SUM2 " " G1 " " G2 " " G3 " " G4 " " G5 " " FUEL " " D1 " " D2')
    w("    close f1 f2")
    w("    stop run.")
    for p, x, k in paras:
        w("%s." % p)
        w("    add %d to %s." % (k, x))
    print("\n".join(out))


main()
