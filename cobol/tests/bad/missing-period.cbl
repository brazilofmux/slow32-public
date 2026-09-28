identification division.
program-id. noperiod.
*> A sentence missing its period, then a paragraph: the error is
*> reported once, and the next paragraph is still found (cobol
*> ISSUES-41; recovery stops at a paragraph header, not only at a
*> period, so P2's own mistake is reported too).
data division.
working-storage section.
01  a               pic 9(4).
procedure division.
p1.
    move 1 to nothing-here
    display a
p2.
    add 1 to undeclared-too.
    stop run.
