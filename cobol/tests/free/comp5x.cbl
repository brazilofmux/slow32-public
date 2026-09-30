*> COMP-5 with a PICTURE of X's (Micro Focus; docs/usage.md):
*> default dialect.  n bytes, unsigned, in the machine's order, holding what its
*> bytes hold -- 258 is 02 01 here, 250 + 10 in one byte wraps to 4.
identification division.
program-id. comp5x.
data division.
working-storage section.
01 a pic xx comp-5 value 258.
01 ar redefines a pic xx.
01 b pic x comp-5 value 250.
01 i pic 9.
01 bt pic 999.
procedure division.
    display function length(a) " " a " " b
    perform varying i from 1 by 1 until i > 2
        compute bt = function ord(ar(i:1)) - 1 display bt " " with no advancing
    end-perform
    display space
    add 10 to b display b
    compute a = a * 200 display a
    stop run.
