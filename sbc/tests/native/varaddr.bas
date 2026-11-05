' what VARSEG, VARPTR and SADD name, as bytes (QBasic 1.1 prints these)
TYPE pair
    tag AS INTEGER
    nm AS STRING * 3
    big AS LONG
END TYPE
DECLARE SUB inproc (p%, q$)
DIM sized AS STRING * 4, rec AS pair, recs(3) AS pair
DIM ia%(10), la&(3), sa!(3), da#(3), wa$(3), fa(2) AS STRING * 3, grid%(2, 3)
x% = 258: y& = 16909060: z! = 1.5: d# = 2.5: s$ = "AB"
sized = "wxyz"

PRINT VARSEG(x%) = VARSEG(y&); VARSEG(x%) = VARSEG(s$); VARSEG(x%) = VARSEG(sized); VARSEG(x%) = VARSEG(wa$(1))
PRINT VARPTR(ia%(0)); VARPTR(ia%(1)); VARPTR(ia%(10)); VARSEG(ia%(10)) = VARSEG(ia%(0))
PRINT VARPTR(la&(1)) - VARPTR(la&(0)); VARPTR(sa!(1)) - VARPTR(sa!(0)); VARPTR(da#(1)) - VARPTR(da#(0))
PRINT VARPTR(wa$(1)) - VARPTR(wa$(0)); VARPTR(fa(1)) - VARPTR(fa(0))
PRINT "first subscript fastest"; VARPTR(grid%(1, 0)) - VARPTR(grid%(0, 0)); VARPTR(grid%(0, 1)) - VARPTR(grid%(0, 0)); VARPTR(grid%(2, 3))

PRINT LEN(x%); LEN(y&); LEN(z!); LEN(d#); LEN(rec); LEN(ia%(1)); LEN(da#(1)); LEN(rec.nm); LEN(sized)

' with no DEF SEG the program's variables are what is reached
PRINT PEEK(VARPTR(x%)); PEEK(VARPTR(x%) + 1)
POKE VARPTR(x%) + 1, 5: PRINT x%
DEF SEG: PRINT PEEK(VARPTR(x%))
FOR i = 0 TO 3: PRINT PEEK(VARPTR(y&) + i); : NEXT: PRINT
FOR i = 0 TO 3: PRINT PEEK(VARPTR(z!) + i); : NEXT: PRINT
FOR i = 0 TO 7: PRINT PEEK(VARPTR(d#) + i); : NEXT: PRINT
POKE VARPTR(z!) + 3, 192: PRINT z!
POKE VARPTR(d#) + 7, 192: PRINT d#
POKE VARPTR(y&), 255: PRINT y&
FOR i = 0 TO 3: PRINT PEEK(VARPTR(sized) + i); : NEXT: PRINT
POKE VARPTR(sized) + 1, 65: PRINT sized

' a string: its descriptor, its characters, its own after a POKE
PRINT PEEK(VARPTR(s$)); PEEK(VARPTR(s$) + 1); (SADD(s$) AND &HFFFF&) = PEEK(VARPTR(s$) + 2) + 256& * PEEK(VARPTR(s$) + 3)
PRINT PEEK(SADD(s$)); PEEK(SADD(s$) + 1)
POKE SADD(s$), 90: PRINT s$
t$ = s$: POKE SADD(s$), 89: PRINT s$; " "; t$
POKE VARPTR(s$), 1: PRINT s$; LEN(s$)
POKE VARPTR(s$), 2: PRINT s$; LEN(s$)
PRINT SADD(wa$(2)); SADD(never$)
wa$(1) = "HI": PRINT PEEK(VARPTR(wa$(1))); PEEK(VARPTR(wa$(1)) + 1); PEEK(SADD(wa$(1)))
POKE SADD(wa$(1)), 74: PRINT wa$(1)

' a segment for each array
ia%(1) = 513: la&(2) = 67305985: sa!(1) = 1: da#(1) = 1
DEF SEG = VARSEG(ia%(1))
PRINT PEEK(VARPTR(ia%(1))); PEEK(VARPTR(ia%(0)) + 3)
POKE VARPTR(ia%(0)) + 4, 7: PRINT ia%(2)
DEF SEG = VARSEG(la&(0)): PRINT PEEK(VARPTR(la&(0)) + 9)
DEF SEG = VARSEG(sa!(0)): PRINT PEEK(VARPTR(sa!(1)) + 3)
POKE VARPTR(sa!(1)) + 3, 192: PRINT sa!(1)
DEF SEG = VARSEG(da#(0)): PRINT PEEK(VARPTR(da#(1)) + 7)
fa(1) = "abc": DEF SEG = VARSEG(fa(0)): PRINT PEEK(VARPTR(fa(1)) + 2)
POKE VARPTR(fa(1)), 122: PRINT fa(1)
grid%(1, 2) = 1027
DEF SEG = VARSEG(grid%(0, 0)): PRINT PEEK(2 * (1 + 3 * 2)); PEEK(2 * (1 + 3 * 2) + 1)

' a record: its fields one after another
rec.tag = 772: rec.nm = "pqr": rec.big = 1
PRINT VARPTR(rec.nm) - VARPTR(rec); VARPTR(rec.big) - VARPTR(rec); VARSEG(rec.big) = VARSEG(rec)
DEF SEG = VARSEG(rec)
PRINT PEEK(VARPTR(rec) + 1); PEEK(VARPTR(rec) + 2); PEEK(VARPTR(rec) + 5)
POKE VARPTR(rec) + 3, 88: PRINT rec.nm
POKE VARPTR(rec), 6: PRINT rec.tag
recs(1).big = 9: recs(1).nm = "klm"
PRINT VARPTR(recs(1)) - VARPTR(recs(0)); VARPTR(recs(1).big) - VARPTR(recs(1))
DEF SEG = VARSEG(recs(0)): PRINT PEEK(VARPTR(recs(0)) + 9 + 5); PEEK(VARPTR(recs(1).nm))
POKE VARPTR(recs(1).big), 200: PRINT recs(1).big

' a dynamic array, past 32767 bytes, gone and made again
REDIM tall%(1 TO 26000)
PRINT VARPTR(tall%(1)); VARPTR(tall%(26000)); VARSEG(tall%(26000)) = VARSEG(tall%(1))
DEF SEG = VARSEG(tall%(1)): POKE VARPTR(tall%(20000)), 9: PRINT tall%(20000)
ERASE tall%
REDIM tall%(4): PRINT VARPTR(tall%(2))
DEF SEG = VARSEG(tall%(0)): POKE 4, 3: PRINT tall%(2)

' procedures: a local, a parameter by reference, an array of its own
DEF SEG
n% = 5: m$ = "mn"
inproc n%, m$
PRINT n%; m$
PRINT "done"
SYSTEM

SUB inproc (p%, q$)
    STATIC keep%
    DIM mine%(3)
    own% = 772: r$ = "hey": keep% = 257
    mine%(1) = 515
    PRINT VARSEG(mine%(0)) = VARSEG(own%); VARPTR(mine%(0)); VARSEG(keep%) = VARSEG(own%); VARSEG(r$) = VARSEG(own%)
    DEF SEG = VARSEG(mine%(1)): PRINT PEEK(VARPTR(mine%(1)))
    DEF SEG = VARSEG(own%)
    PRINT PEEK(VARPTR(own%)); PEEK(SADD(r$)); PEEK(VARPTR(keep%) + 1)
    POKE VARPTR(own%), 1: PRINT own%
    POKE VARPTR(p%), 9
    POKE SADD(q$), 113
    DEF SEG
END SUB
