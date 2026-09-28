identification division.
program-id. rwindex.
*> A reserved word never names an index; cobol ISSUES-43.
data division.
working-storage section.
01  t.
    05 e pic x occurs 3 indexed by count.
procedure division.
    stop run.
