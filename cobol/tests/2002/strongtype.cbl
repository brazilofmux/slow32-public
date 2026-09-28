identification division.
program-id. strongtype.
*> Strongly-typed groups (2023 13.18.58 STRONG, 8.5.3.3, 8.8.4.2.12,
*> D.8.3; cobol ISSUES-80).  A group described by a STRONG type, or a
*> group inside one, moves only to and from a group of the same type,
*> and two such groups compare element by element in order, each by its
*> own rules: -1.00 in PIC S9(3)V99 is the bytes 0010p and 0.50 is
*> 00050, so as bytes -1.00 would be the greater; as numbers it is the
*> less.  A strong type may hold a strong type; elementary items inside
*> are used freely.
*> No oracle (docs/typedef.md).
data division.
working-storage section.
01  date-t typedef strong.
    05 yy        pic 9999.
    05 mm        pic 99.
    05 dd        pic 99.
01  money-t is typedef strong.
    05 amt       pic s9(3)v99.
    05 cur       pic x(3).
01  event-t typedef strong.
    05 on-date   type to date-t.
    05 cost      type to money-t.
01  d1           type to date-t.
01  d2           type to date-t.
01  m1           type to money-t.
01  m2           type to money-t.
01  ev           type to event-t.
procedure division.
main.
    move 2026 to yy of d1  move 9 to mm of d1  move 28 to dd of d1
    move d1 to d2
    display "moved: " yy of d2 "-" mm of d2 "-" dd of d2
    if d1 = d2 display "d1 = d2" end-if
    move 29 to dd of d2
    if d1 < d2 display "d1 < d2" end-if
    move -1 to amt of m1  move "USD" to cur of m1
    move 0.50 to amt of m2  move "USD" to cur of m2
    if m1 < m2 display "-1.00 < 0.50: element by element, not bytes" end-if
    move d1 to on-date of ev
    move m2 to cost of ev
    display "event: " yy of on-date of ev " " amt of cost of ev " " cur of cost of ev
    initialize m1
    display "initialized: " amt of m1 " [" cur of m1 "]"
    stop run.
