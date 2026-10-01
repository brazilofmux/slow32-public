#!/usr/bin/env python3
"""Generate a random COBOL 85 program of control flow, for differential
testing against GnuCOBOL and between compilers (GEN=flow run-gen.sh ...).

    gen-flow.py SEED [PARAGRAPHS] > prog.cbl

The front-end pass (docs/plans/frontend-pass.md) lays conditional code out
from nodes -- an IF whose branch is one jump becomes the condition's own
branch, a phrase's statements a block placed where the statement wants
them -- so this exercises exactly that, and prints a trace a layout
mistake would change:
- paragraphs that DISPLAY their name as they are entered, reached by
  falling through and by GO TO, only ever forward, so every program ends;
- IF with GO TO, NEXT SENTENCE, CONTINUE, nested IFs, ELSE that is a GO
  TO, combined conditions;
- ADD / SUBTRACT / MULTIPLY / COMPUTE into small pictures, so ON SIZE
  ERROR fires, with GO TO, DISPLAY or CONTINUE in either phrase;
- EVALUATE of an item, an expression or TRUE, WHEN bodies that are GO
  TO, DISPLAY or CONTINUE, THRU ranges, WHEN OTHER or none;
- PERFORM loops, inline and of paragraphs (Q1..Q4, THRU): TIMES, UNTIL,
  VARYING up and down, WITH TEST AFTER, VARYING ... AFTER, nested
  inline loops, a GO TO out of an inline body; every loop is counted,
  so every program ends;
- a sequential file written and read back in a loop, AT END GO TO and
  NOT AT END;
- CALL of a program that does not exist, ON EXCEPTION GO TO / DISPLAY,
  NOT ON EXCEPTION showing CALLED NOSUCHPROG -- never right, as the
  program is not there; GnuCOBOL shows it after an ON EXCEPTION phrase
  that falls through (docs/oracles.md), which run-gen.sh counts apart.
"""
import random
import sys


def main():
    seed = int(sys.argv[1])
    npara = int(sys.argv[2]) if len(sys.argv) > 2 else 30
    r = random.Random(seed)
    out = []
    w = out.append

    nvar = 4
    w("IDENTIFICATION DIVISION.")
    w("PROGRAM-ID. GENFLOW.")
    w("ENVIRONMENT DIVISION.")
    w("INPUT-OUTPUT SECTION.")
    w("FILE-CONTROL.")
    w('    SELECT F ASSIGN TO "genflow.dat" ORGANIZATION IS SEQUENTIAL.')
    w("DATA DIVISION.")
    w("FILE SECTION.")
    w("FD  F.")
    w("01  F-REC PIC 9(3).")
    w("WORKING-STORAGE SECTION.")
    for i in range(nvar):
        w("01  V%d PIC S9(3) VALUE %d." % (i, r.randint(-99, 99)))
    w("01  S2 PIC 9(2) VALUE 0.")         # small: SIZE ERROR fires
    w("01  CNT PIC 9(3) VALUE 0.")
    w("01  K PIC 9(3) VALUE 0.")
    for i in range(1, 4):
        w("01  L%d PIC S9(3) VALUE 0." % i)    # loop items: only the loops change them
    w("01  T1 PIC S9(3) VALUE 0.")
    w("01  U1 PIC S9(3) VALUE 0.")            # counted by the performed paragraphs, for their UNTIL loops
    w("PROCEDURE DIVISION.")

    def var():
        return "V%d" % r.randrange(nvar)

    def lit():
        return str(r.randint(-50, 50))

    def cond():
        ops = ["=", "<", ">", "NOT =", "NOT <", "NOT >", ">=", "<="]
        c = "%s %s %s" % (var(), r.choice(ops), r.choice([var(), lit()]))
        k = r.random()
        if k < 0.2:
            c = "%s AND %s %s %s" % (c, var(), r.choice(ops), lit())
        elif k < 0.35:
            c = "%s OR %s %s %s" % (c, var(), r.choice(ops), lit())
        elif k < 0.45:
            c = "NOT (%s)" % c
        return c

    def target(i):
        return "P%d" % r.randint(i + 1, npara)

    def simple(i, depth=0):
        """one imperative statement (no period)"""
        k = r.random()
        if k < 0.3:
            return 'DISPLAY "D%d-%d " %s' % (i, r.randrange(1000), var())
        if k < 0.5:
            return "ADD %s TO %s" % (lit(), var())
        if k < 0.6:
            return "CONTINUE"
        if k < 0.75 and depth < 2:
            return nested_if(i, depth + 1)
        return "MOVE %s TO %s" % (lit(), var())

    def phrase(i):
        k = r.random()
        if k < 0.4:
            return "GO TO %s" % target(i)
        if k < 0.7:
            return 'DISPLAY "E%d-%d"' % (i, r.randrange(1000))
        return "CONTINUE"

    def nested_if(i, depth):
        s = "IF %s %s" % (cond(), simple(i, depth))
        if r.random() < 0.5:
            s += " ELSE %s" % simple(i, depth)
        return s + " END-IF"

    def size_stmt(i):
        k = r.random()
        if k < 0.25:
            s = "ADD %d TO S2" % r.randint(1, 60)
        elif k < 0.5:
            s = "SUBTRACT %d FROM S2" % r.randint(1, 60)
        elif k < 0.75:
            s = "MULTIPLY %d BY S2" % r.randint(1, 9)
        else:
            s = "COMPUTE S2 = S2 * %d + %d" % (r.randint(1, 5), r.randint(0, 30))
        has_on = r.random() < 0.8
        if has_on:
            s += " ON SIZE ERROR %s" % phrase(i)
        if r.random() < 0.5 or not has_on:
            s += " NOT ON SIZE ERROR %s" % phrase(i)
        end = {"A": "END-ADD", "S": "END-SUBTRACT", "M": "END-MULTIPLY", "C": "END-COMPUTE"}[s[0]]
        return s + " " + end + ' DISPLAY "S2 " S2'

    def evaluate(i):
        kind = r.random()
        if kind < 0.4:
            s = "EVALUATE %s" % var()
            def obj():
                a = r.randint(-40, 40)
                return str(a) if r.random() < 0.6 else "%d THRU %d" % (a, a + r.randint(0, 30))
        elif kind < 0.6:
            s = "EVALUATE %s + %s" % (var(), lit())
            def obj():
                return str(r.randint(-60, 60))
        else:
            s = "EVALUATE TRUE"
            def obj():
                return cond()
        for _ in range(r.randint(1, 4)):
            s += " WHEN %s" % obj()
            if r.random() < 0.2:
                s += " WHEN %s" % obj()        # two WHENs, one body
            s += " %s" % phrase(i)
        if r.random() < 0.6:
            s += " WHEN OTHER %s" % phrase(i)
        return s + " END-EVALUATE"

    def varying(lv):
        # n < 0: the condition holds at the start, and the body is not
        # executed at all (unless the test is after)
        if r.random() < 0.75:
            a = r.randint(-2, 3); n = r.randint(-2, 4); by = r.randint(1, 3)
            return "VARYING %s FROM %d BY %d UNTIL %s > %d" % (lv, a, by, lv, a + n)
        a = r.randint(0, 5); n = r.randint(-2, 4); by = r.randint(1, 2)
        return "VARYING %s FROM %d BY -%d UNTIL %s < %d" % (lv, a, by, lv, a - n)

    def test_phrase():
        k = r.random()
        return "WITH TEST AFTER " if k < 0.25 else "WITH TEST BEFORE " if k < 0.35 else ""

    def loop_body(i, depth):
        lv = "L%d" % depth
        st = ['DISPLAY "B%d-%d " %s' % (i, r.randrange(1000), lv)]
        for _ in range(r.randint(0, 2)):
            k = r.random()
            if k < 0.45:
                st.append(simple(i))
            elif k < 0.6 and depth < 3:
                st.append(inline_loop(i, depth + 1))
            elif k < 0.75 and i < npara:
                st.append(evaluate(i))
            elif k < 0.85 and i < npara:
                st.append("IF %s GO TO %s END-IF" % (cond(), target(i)))
            else:
                st.append("ADD 1 TO T1")
        return " ".join(st)

    def inline_loop(i, depth=1):
        lv = "L%d" % depth
        k = r.random()
        if k < 0.3:
            return "PERFORM %d TIMES %s END-PERFORM" % (r.randint(0, 4), loop_body(i, depth))
        if k < 0.4:
            return "MOVE %d TO %s PERFORM %s TIMES %s END-PERFORM" % (r.randint(-1, 3), lv, lv, loop_body(i, depth).replace("ADD 1 TO T1", "CONTINUE"))
        if k < 0.7:
            return "PERFORM %s%s %s END-PERFORM" % (test_phrase(), varying(lv), loop_body(i, depth))
        return "MOVE 0 TO %s PERFORM %sUNTIL %s >= %d %s ADD 1 TO %s END-PERFORM" % (lv, test_phrase(), lv, r.randint(0, 4), loop_body(i, depth), lv)

    def para_loop():
        rng = "Q%d" % r.randint(1, 4)
        if r.random() < 0.3:
            a = r.randint(1, 3); rng = "Q%d THRU Q%d" % (a, r.randint(a, 4))
        k = r.random()
        if k < 0.2:
            return "PERFORM %s" % rng
        if k < 0.4:
            return "PERFORM %s %d TIMES" % (rng, r.randint(0, 3))
        if k < 0.6:
            return "MOVE 0 TO U1 PERFORM %s %sUNTIL U1 >= %d" % (rng, test_phrase(), r.randint(0, 3))
        if k < 0.8:
            return "PERFORM %s %s%s" % (rng, test_phrase(), varying("L1"))
        return "PERFORM %s %s%s AFTER %s" % (rng, test_phrase(), varying("L1"), varying("L2")[len("VARYING "):])

    # the file: written, then read in a loop
    w("P0.")
    w('    DISPLAY "P0"')
    w("    OPEN OUTPUT F")
    n = r.randint(0, 5)
    w("    PERFORM %d TIMES ADD 1 TO K MOVE K TO F-REC WRITE F-REC END-PERFORM" % n)
    w("    CLOSE F")
    w("    OPEN INPUT F.")
    w("RDLOOP.")
    w("    READ F AT END GO TO RDONE NOT AT END ADD 1 TO CNT END-READ")
    w('    DISPLAY "READ " F-REC')
    w("    GO TO RDLOOP.")
    w("RDONE.")
    w('    CLOSE F DISPLAY "READ COUNT " CNT.')
    for i in range(1, npara + 1):
        w("P%d." % i)
        w('    DISPLAY "P%d"' % i)
        for _ in range(r.randint(1, 4)):
            k = r.random()
            if k < 0.25:
                # IF ... GO TO, with or without ELSE
                s = "    IF %s GO TO %s" % (cond(), target(i)) if i < npara else "    IF %s CONTINUE" % cond()
                if r.random() < 0.4:
                    s += " ELSE %s" % simple(i)
                w(s + " END-IF")
            elif k < 0.35:
                w("    IF %s %s ELSE GO TO %s END-IF" % (cond(), simple(i), target(i)) if i < npara
                  else "    IF %s %s END-IF" % (cond(), simple(i)))
            elif k < 0.45:
                w("    IF %s CONTINUE ELSE %s END-IF" % (cond(), simple(i)))
            elif k < 0.55:
                # NEXT SENTENCE: the sentence ends here, so the period follows
                w("    IF %s NEXT SENTENCE ELSE %s." % (cond(), simple(i)))
                w('    DISPLAY "after-ns %d"' % i)
            elif k < 0.68:
                w("    " + size_stmt(i) if i < npara else "    ADD 1 TO S2")
            elif k < 0.76 and i < npara:
                w("    " + evaluate(i))
            elif k < 0.84:
                w("    " + inline_loop(i))
            elif k < 0.9:
                w("    " + para_loop())
            elif k < 0.8 and i < npara:
                w('    CALL "NOSUCHPROG" ON EXCEPTION %s NOT ON EXCEPTION DISPLAY "CALLED NOSUCHPROG" END-CALL' % phrase(i))
            else:
                w("    " + simple(i))
        w("    .")
    w("PEND.")
    w('    DISPLAY "END " V0 " " V1 " " S2 " " T1')
    w("    STOP RUN.")
    # performed paragraphs: no GO TO out of them; each counts U1, which ends their UNTIL loops
    for q in range(1, 5):
        w("Q%d." % q)
        w('    DISPLAY "Q%d " L1 " " L2' % q)
        for _ in range(r.randint(0, 2)):
            w("    " + simple(npara + 1))
        w("    ADD 1 TO U1")
        w("    .")
    print("\n".join(out))


main()
