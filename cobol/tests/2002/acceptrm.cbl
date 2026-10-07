*> ACCEPT into a reference-modified item: the part takes the value,
*> what stands beside it is kept (2023 14.9.1.4 rule 1: the receiving
*> operand's size; 8.4.3.3: the unique data item).  Every FROM form passed
*> the whole item's descriptor at the part's address once, so ACCEPT
*> X(2:2) FROM TIME wrote four digits (found by queue item
*> 24).  The clock is pinned by acceptrm.env.  GnuCOBOL agrees.
*> docs/conformance/accept.md
identification division.
program-id. acceptrm.
data division.
working-storage section.
01 x pic x(8) value "abcdefgh".
01 y pic x(4) value "wxyz".
01 i pic 9 value 2.
procedure division.
    accept x(2:2) from time
    display x y
    accept x(5:3) from date yyyymmdd
    display x y
    accept x(i:i) from day
    display x y
    accept x(7:) from day-of-week
    display x y
    stop run.
