#!/usr/bin/env python3
"""Generate a random program of out-of-line PERFORMs as badly behaved as
the old programs are (GEN=perf tests/gen/run-self.sh REV ...).

    gen-perf.py SEED [PARAGRAPHS] > prog.cbl

The runtime keeps the PERFORMs under way as frames, and what it does
with them where the standard stops talking is its own ruling, written
for programs that exist: a GO TO out of a performed range abandons the
frame when the range is performed again, or when control reaches the
exit of a range performed earlier; an exit nobody is waiting on falls
through; a called program's frames are its own, and what it leaves
behind goes when it returns.  A change to how the frames are kept must
keep all of that, so this writes programs that do nothing else:

- paragraphs and sections that DISPLAY their name as they are entered;
- PERFORM of a paragraph, of a section, THRU a later paragraph, n TIMES;
- GO TO anywhere, forward or back, in or out of whatever is being
  performed, on a condition that changes as the program runs;
- falling through the end of a range that is, or is not, being
  performed;
- two contained programs, one of them often larger than the program
  that holds it, and a second program in the same file, each with
  paragraphs of its own, called from inside PERFORMs, performing,
  jumping and leaving by EXIT PROGRAM from the middle of a PERFORM
  (paragraph numbers begin again at each program in a file, and
  contained programs that are siblings share theirs: an exit must be
  its own program's);
- in some programs, the program calling itself (RECURSIVE), so that one
  activation's frames lie under another's on the same paragraphs.

Every paragraph counts a step and the run stops at a limit, so every
program ends; the trace is what is compared.  The compiler and runtime
before the change are the oracle (run-self.sh): the standard leaves
most of this undefined, so GnuCOBOL is not asked.
"""
import random
import sys

LIMIT = 600


def body(r, w, names, sections, k, calls, recursive, indent="    "):
    """a paragraph's statements"""
    for _ in range(r.randint(1, 4)):
        c = r.randrange(20)
        a = r.choice(names)
        if c < 3:
            w("%sPERFORM %s" % (indent, a))
        elif c < 5:
            i = r.randrange(len(names)); j = r.randint(i, min(len(names) - 1, i + 3))
            w("%sPERFORM %s THRU %s" % (indent, names[i], names[j]))
        elif c == 5 and sections:
            w("%sPERFORM %s" % (indent, r.choice(sections)))
        elif c == 6:
            w("%sPERFORM %s %d TIMES" % (indent, a, r.randint(1, 2)))
        elif c < 11:
            w("%sIF V < %d GO TO %s." % (indent, r.randint(10, 90), a))
        elif c == 11:
            w("%sIF V > %d PERFORM %s ELSE GO TO %s." % (indent, r.randint(10, 90), a, r.choice(names)))
        elif c < 15 and calls:
            w("%s%s" % (indent, r.choice(calls)))
        elif c < 18 and recursive:
            w("%sIF DEPTH < 4 ADD 1 TO DEPTH CALL \"GENPERF\" SUBTRACT 1 FROM DEPTH." % indent)
        elif c == 18:
            w("%sCOMPUTE V = FUNCTION MOD(V * 7 + %d, 101)" % (indent, r.randint(1, 50)))
        else:
            w('%sDISPLAY "%s-%d " V' % (indent, k, r.randint(0, 9)))


def paragraphs(r, w, prefix, n, calls, recursive, leave):
    """n paragraphs, some gathered into sections"""
    names = ["%s%d" % (prefix, i) for i in range(1, n + 1)]
    sections = []
    # the names of the sections are known before any body is written
    sec_at = {}
    i = 1
    while i <= n:
        if r.random() < 0.25:
            s = "%sS%d" % (prefix, len(sections) + 1); sections.append(s); sec_at[i] = s
            i += r.randint(2, 3)
        else:
            i += 1
    for i, name in enumerate(names, 1):
        if i in sec_at:
            w("%s SECTION." % sec_at[i])
        w("%s." % name)
        w("    ADD 1 TO STEPS")
        w('    IF STEPS > %d DISPLAY "LIMIT" STOP RUN.' % LIMIT)
        w("    COMPUTE V = FUNCTION MOD(V * 31 + %d, 101)" % i)
        w('    DISPLAY "%s " V' % name)
        if leave and r.random() < 0.3:
            w("    IF V < %d %s." % (r.randint(20, 60), leave))
        body(r, w, names, sections, name, calls, recursive)
        w("    CONTINUE.")


def subprogram(r, w, name, prefix, n, calls, header, footer):
    """a program of n paragraphs that performs, jumps, and leaves from the middle"""
    for l in header:
        w(l)
    w("%s-MAIN." % prefix)
    w('    DISPLAY "%s"' % name)
    w("    PERFORM %s1 THRU %s%d" % (prefix, prefix, r.randint(1, n)))
    w("    PERFORM %s%d" % (prefix, r.randint(1, n)))
    w('    DISPLAY "%s DONE"' % name)
    w("    EXIT PROGRAM.")
    paragraphs(r, w, prefix, n, calls, False, "EXIT PROGRAM")
    w("%sEND." % prefix)
    w("    EXIT PROGRAM.")
    for l in footer:
        w(l)


def main():
    seed = int(sys.argv[1])
    r = random.Random(seed * 104729 + 3)
    # few paragraphs: the same ranges are met again and again, in every state
    npara = min(int(sys.argv[2]) if len(sys.argv) > 2 else 12, r.randint(4, 12))
    recursive = r.random() < 0.5
    out = []
    w = out.append
    w("IDENTIFICATION DIVISION.")
    w("PROGRAM-ID. GENPERF%s." % (" IS RECURSIVE" if recursive else ""))
    w("DATA DIVISION.")
    w("WORKING-STORAGE SECTION.")
    w("01  STEPS PIC 9(4) COMP VALUE 0 GLOBAL.")
    w("01  V PIC 9(4) COMP VALUE %d GLOBAL." % r.randint(0, 100))
    w("01  DEPTH PIC 9(4) COMP VALUE 0.")
    w("PROCEDURE DIVISION.")
    w("MAIN-PARA.")
    w('    DISPLAY "MAIN " DEPTH')
    names = ["P%d" % i for i in range(1, npara + 1)]
    for _ in range(r.randint(2, 4)):
        c = r.randrange(4)
        if c == 0:
            i = r.randrange(npara); j = r.randint(i, min(npara - 1, i + 4))
            w("    PERFORM %s THRU %s" % (names[i], names[j]))
        elif c == 1:
            w("    PERFORM %s %d TIMES" % (r.choice(names), r.randint(1, 3)))
        else:
            w("    PERFORM %s" % r.choice(names))
    w('    DISPLAY "MAIN DONE " DEPTH')
    w("    IF DEPTH > 0 EXIT PROGRAM.")
    w("    STOP RUN.")
    calls = ['CALL "SUB1"', 'CALL "SUB2"', 'CALL "GENPERF2" USING STEPS V']
    paragraphs(r, w, "P", npara, calls, recursive, "EXIT PROGRAM" if recursive else None)
    w("PEND.")
    w('    DISPLAY "END " DEPTH')
    w("    IF DEPTH > 0 EXIT PROGRAM.")
    w("    STOP RUN.")
    # the contained programs: siblings, the first often the larger; SUB2 is COMMON, so SUB1 may call it
    subprogram(r, w, "SUB1", "Q", r.randint(3, 16), ['CALL "SUB2"'],
               ["IDENTIFICATION DIVISION.", "PROGRAM-ID. SUB1.", "PROCEDURE DIVISION."], ["END PROGRAM SUB1."])
    subprogram(r, w, "SUB2", "R", r.randint(3, 8), [],
               ["IDENTIFICATION DIVISION.", "PROGRAM-ID. SUB2 IS COMMON.", "PROCEDURE DIVISION."], ["END PROGRAM SUB2."])
    w("END PROGRAM GENPERF.")
    # a second program in the file: its paragraphs numbered from the start again
    subprogram(r, w, "GENPERF2", "T", r.randint(3, 10), [],
               ["IDENTIFICATION DIVISION.", "PROGRAM-ID. GENPERF2.", "DATA DIVISION.", "LINKAGE SECTION.",
                "01  STEPS PIC 9(4) COMP.", "01  V PIC 9(4) COMP.", "PROCEDURE DIVISION USING STEPS V."],
               ["END PROGRAM GENPERF2."])
    print("\n".join(out))


main()
