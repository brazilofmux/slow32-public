identification division.
program-id. lenrefmod.
*> FUNCTION LENGTH of a reference modification whose length is computed
*> (cobol ISSUES-81): counted when the statement runs, the length given,
*> or the rest of the item from a computed start.  GnuCOBOL returns the
*> whole item's length, 10, for all four: lenrefmod.oracle-expected, a
*> documented divergence (docs/oracles.md).
data division.
working-storage section.
01  a        pic x(10) value "abcdefghij".
01  s        pic 99 value 3.
01  l        pic 99 value 4.
procedure division.
main.
    display function length(a(s:l))
    display function length(a(2:l + 1))
    display function length(a(s:))
    move 7 to s
    display function length(a(s:))
    stop run.
