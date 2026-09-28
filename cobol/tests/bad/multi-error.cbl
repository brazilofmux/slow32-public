identification division.
program-id. multierr.
*> Six mistakes, six messages (cobol ISSUES-41): an error in a data
*> entry or a sentence is reported and the compile goes on at the next
*> one.  B's entry is dropped, so the sentence that uses it is dropped
*> without a second message; T keeps its shape with its item gone.  The
*> VALUE error is reported after the index one: values are checked when
*> the data division is finished.
data division.
working-storage section.
01  a               pic 9(4).
01  b               pic q(4).
01  c               pic x(3) value "toolong".
01  t.
    05 e            pic x occurs 3 indexed by count.
procedure division.
main.
    move b to a.
    add 1 to nothing-here.
    move 1 to a
    end-if.
    compute a = a + .
    display a.
    stop run.
