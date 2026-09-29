identification division.
program-id. bref.
*> A bit data item passed BY REFERENCE starts a byte (2023 14.9.4.3 rule
*> 6; cobol ISSUES-94 B6): b is bits 4-6 of its byte.
data division.
working-storage section.
01  r.
    05 a pic 1(3) usage bit.
    05 b pic 1(3) usage bit.
procedure division.
    call "sub" using b
    stop run.
