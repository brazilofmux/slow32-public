identification division.
program-id. impliedpic.
*> A PICTURE implied by the VALUE literal (2023 13.16.3 rule 9;
*> standard-queue item 11): X(length) for an alphanumeric literal,
*> 1(length) for a boolean one, N(length) for a national one; under a
*> group too.  No oracle: GnuCOBOL 4 requires the PICTURE clause.
data division.
working-storage section.
01  greeting    value "hello, world".
01  flags       value b"1011".
01  nat         value n"abc".
01  grp.
    05  a       value "xy".
    05  b       pic 9(3) value 7.
procedure division.
    display "[" greeting "] " function length(greeting)
    display flags " " function length(flags)
    display function display-of(nat) " " function length(nat)
    display grp " " function length(grp)
    stop run.
end program impliedpic.
