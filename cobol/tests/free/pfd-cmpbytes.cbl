*> -fnumproc=pfd (the harness gives it to pfd-* programs): a numeric item
*> holds what its picture says, so a MOVE or relation between two items
*> of one description may be done by value, and an item copied to or
*> compared with one that must stay as written can still live in a
*> register.  The two readings differ only on content the standard leaves
*> undefined -- non-digits in a numeric item -- and that is what this
*> program puts there.  Under -fnumproc=pfd those rows read the value
*> (spaces as zeros); without it (cmpbytes.cbl, and GnuCOBOL, whose
*> answer is the .oracle-expected) they compare the bytes.  The user's
*> ruling, 2026-10-27: opt-in; the default carries the bytes.
identification division.
program-id. pfd-cmpbytes.
data division.
working-storage section.
01  raw   pic x(4) value '  12'.
01  a redefines raw pic 9(4).
01  b     pic 9(4) value 12.
01  blanked.
    05  f1    pic 9(4).
    05  f2    pic x(2).
01  n1    pic 9(4) value 7.
01  n2    pic 9(4) value 30.
01  k     pic 9(4) value 0.
procedure division.
main-procedure.
    *> undefined content: blanks in a numeric field of the same picture
    if a = b display 'A equal' else display 'A differs' end-if
    move spaces to blanked
    move 0 to n1
    if n1 = f1 display 'B equal' else display 'B differs' end-if
    *> defined content: both readings agree, and so must we
    move 1234 to f1
    move f1 to n1
    if n1 = f1 display 'C equal' else display 'C differs' end-if
    if n2 > n1 display 'D greater' else display 'D not-greater' end-if
    perform varying k from 1 by 1 until k > 3
        add k to n2
    end-perform
    move n2 to f1
    display 'E ' f1 ' ' n2
    stop run.
