*> Comparisons of national operands under a PROGRAM COLLATING SEQUENCE:
*> the alphabet is an alphanumeric collating sequence, so national
*> comparisons keep the native national order, the code units (2023
*> 8.8.4.2.6, 8.8.4.2.9; the SORT side is 2002/natsortcoll).  Relations national to
*> national, to a literal and to an alphanumeric item, EVALUATE, a
*> condition-name range, HIGH-VALUE and LOW-VALUE, SEARCH ALL, and an
*> alphanumeric control that the alphabet does order (11).  Every line
*> is T, or the entry found.
*> No oracle: GnuCOBOL's USAGE NATIONAL is unfinished; checked by hand.
identification division.
program-id. natifcoll.
environment division.
configuration section.
object-computer. probe-box program collating sequence is flip.
special-names.
    alphabet flip is "y" "x".
data division.
working-storage section.
01 nx pic n(1) value n"x".
01 ny pic n(1) value n"y".
01 nw pic n(1) value n"w".
01 ne pic n(1) value n"é".
01 ax pic x(1) value "x".
01 ay pic x(1) value "y".
01 nk pic n(1).
   88 in-range value n"w" thru n"y".
01 nh pic n(1).
01 tbl.
   05 ent occurs 4 ascending key ek indexed by ix.
      10 ek pic n(1).
      10 ev pic x(4).
procedure division.
    if nx < ny display "1 T" else display "1 F" end-if
    if nx < n"y" display "2 T" else display "2 F" end-if
    if nx < ay display "3 T" else display "3 F" end-if
    if ax < ny display "4 T" else display "4 F" end-if
    evaluate true when nw < nx display "5 T" when other display "5 F" end-evaluate
    move n"x" to nk
    if in-range display "6 T" else display "6 F" end-if
    if ny < ne display "7 T" else display "7 F" end-if
    if ne < high-value display "8 T" else display "8 F" end-if
    if low-value < nw display "9 T" else display "9 F" end-if
    move high-value to nh
    if nh = high-value display "10 T" else display "10 F" end-if
    if ay < ax display "11 T" else display "11 F" end-if
    move n"w" to ek(1) move "ww" to ev(1)
    move n"x" to ek(2) move "xx" to ev(2)
    move n"y" to ek(3) move "yy" to ev(3)
    move n"é" to ek(4) move "ee" to ev(4)
    search all ent at end display "12 none"
        when ek(ix) = n"x" display "12 " ev(ix) end-search
    search all ent at end display "13 none"
        when ek(ix) = n"y" display "13 " ev(ix) end-search
    stop run.
