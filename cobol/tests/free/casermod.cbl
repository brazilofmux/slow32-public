identification division.
program-id. casermod.
*> UPPER-CASE and LOWER-CASE of a reference-modified argument return as
*> many characters as the reference modification selects, not the whole
*> item (cobol ISSUES-66: the function took the item's size, and read
*> past the selected characters).
data division.
working-storage section.
01  a        pic x(10) value "abcdefghij".
01  u        pic x(10) value "ABCDEFGHIJ".
procedure division.
main.
    display "[" function upper-case(a(2:3)) "]"
    display "[" function lower-case(u(8:3)) "]"
    display function length(function upper-case(a(4:5)))
    stop run.
