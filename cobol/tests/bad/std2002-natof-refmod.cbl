identification division.
program-id. natofrm.
*> A function result whose length is known only at run time is not yet reference-modified.
data division.
working-storage section.
01  a pic x(4) value "abcd".
procedure division.
    display function national-of(a)(1:2)
    stop run.
