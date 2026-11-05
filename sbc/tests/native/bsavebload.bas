' BSAVE and BLOAD: the file's form, where its bytes go, what is refused
ON ERROR GOTO oops
DIM w AS INTEGER, c AS STRING * 1
REDIM a%(100)
FOR i = 0 TO 100: a%(i) = i * 3: NEXT
DEF SEG = VARSEG(a%(0))
BSAVE "BLTEST1.BIN", VARPTR(a%(2)), 10
OPEN "BLTEST1.BIN" FOR BINARY AS #1
PRINT LOF(1);
GET #1, 1, c: PRINT ASC(c);
GET #1, , w: PRINT w = VARSEG(a%(0));
GET #1, , w: PRINT w = VARPTR(a%(2));
GET #1, , w: PRINT w;
FOR i = 8 TO LOF(1): GET #1, i, c: PRINT ASC(c); : NEXT: PRINT
CLOSE #1
FOR i = 0 TO 100: a%(i) = 0: NEXT
BLOAD "BLTEST1.BIN", VARPTR(a%(10))
PRINT a%(10); a%(11); a%(14); a%(15)
BLOAD "BLTEST1.BIN"
PRINT a%(2); a%(3); a%(6); a%(7)
PRINT a%(9); a%(16); a%(8)

' another type, and back through a variable
REDIM g!(4), h&(4)
g!(1) = 1.5: g!(2) = -2.25
DEF SEG = VARSEG(g!(0)): BSAVE "BLTEST2.BIN", 0, 20
DEF SEG = VARSEG(h&(0)): BLOAD "BLTEST2.BIN", 0
PRINT h&(0); h&(1); h&(2)
DEF SEG
x% = 513: BSAVE "BLTEST3.BIN", VARPTR(x%), 2
x% = 0: BLOAD "BLTEST3.BIN": PRINT x%
y% = 0: BLOAD "BLTEST3.BIN", VARPTR(y%): PRINT y%

' plain memory
DEF SEG = &H2000: POKE 0, 1: POKE 1, 2: POKE 2, 3: POKE 3, 4: BSAVE "BLTEST4.BIN", 0, 4
DEF SEG = &H3000: POKE 0, 77: POKE 1, 78
BLOAD "BLTEST4.BIN": PRINT PEEK(0); PEEK(1)
BLOAD "BLTEST4.BIN", 8: PRINT PEEK(8); PEEK(11)
DEF SEG = &H2000: PRINT PEEK(0)

' what QBasic refuses
DEF SEG = &H3000
FOR t = 1 TO 14
    SELECT CASE t
    CASE 1: BLOAD "BLNOPE.BIN"
    CASE 2
        OPEN "BLTEST6.BIN" FOR OUTPUT AS #1: PRINT #1, "not one of ours": CLOSE #1
        BLOAD "BLTEST6.BIN", 0
    CASE 3
        BSAVE "BLTEST6.BIN", 0, 0
        OPEN "BLTEST6.BIN" FOR BINARY AS #1: PRINT LOF(1); : CLOSE #1
    CASE 4: BSAVE "BLTEST6.BIN", 0, 65536
    CASE 5: BSAVE "BLTEST6.BIN", 65536, 2
    CASE 6: BSAVE "BLTEST6.BIN", -1, 2: PRINT "m1";
    CASE 7: BLOAD "BLTEST6.BIN", 65536
    CASE 8: BLOAD "BLTEST6.BIN", -1: PRINT "m1b";
    CASE 9: BSAVE "", 0, 2
    CASE 10: BLOAD ""
    CASE 11
        ' says it has ten, holds two: they load, and then it is refused
        OPEN "BLTEST6.BIN" FOR OUTPUT AS #1
        PRINT #1, CHR$(253); CHR$(0); CHR$(32); CHR$(8); CHR$(0); CHR$(10); CHR$(0); "AB";
        CLOSE #1
        POKE 0, 9: POKE 1, 9: POKE 2, 9
        BLOAD "BLTEST6.BIN", 0
    CASE 12
        PRINT PEEK(0); PEEK(1); PEEK(2);
        OPEN "BLTEST6.BIN" FOR OUTPUT AS #1
        PRINT #1, CHR$(253); CHR$(0); CHR$(32); CHR$(8); CHR$(0); CHR$(1); CHR$(0); "ABCDEF";
        CLOSE #1
        POKE 0, 9: POKE 1, 9
        BLOAD "BLTEST6.BIN", 0
        PRINT PEEK(0); PEEK(1);
    CASE 13
        OPEN "BLTEST6.BIN" FOR OUTPUT AS #1: PRINT #1, CHR$(253); CHR$(0); : CLOSE #1
        BLOAD "BLTEST6.BIN", 0
    CASE 14
        OPEN "BLTEST6.BIN" FOR OUTPUT AS #1: CLOSE #1
        BLOAD "BLTEST6.BIN", 0
    END SELECT
    PRINT t; "fine"
    GOTO after
oops:
    PRINT t; "ERR"; ERR
    RESUME after
after:
NEXT
DEF SEG
KILL "BLTEST1.BIN": KILL "BLTEST2.BIN": KILL "BLTEST3.BIN"
KILL "BLTEST4.BIN": KILL "BLTEST6.BIN"
PRINT "done"
SYSTEM
