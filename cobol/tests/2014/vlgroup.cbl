identification division.
program-id. vlgroup.
*> Variable-length groups moved and compared whole (COBOL 2014; 2023
*> 8.5.1.12 compatibility, 14.6.9.2-3): a group holding a dynamic-capacity
*> table and a dynamic-length item moved to a group of the same shape --
*> the receiver takes the sender's capacity and elements, the item's
*> content and length, the fixed bytes between them; compared whole, the
*> shorter table or item runs against spaces; dynamic-length items under
*> a fixed OCCURS are parts of the shape too.  No oracle: GnuCOBOL 4 has
*> neither clause.  No gcobol either.
data division.
working-storage section.
01 g1.
   05 h1 pic x(2) value "aa".
   05 t1 occurs dynamic capacity in c1.
      10 k1 pic 9(2).
   05 s1 pic x dynamic length.
   05 z1 pic x(2) value "zz".
01 g2.
   05 h2 pic x(2) value "bb".
   05 t2 occurs dynamic capacity in c2 to 9.
      10 k2 pic 9(2).
   05 s2 pic x dynamic length limit 20.
   05 z2 pic x(2) value "yy".
01 g3.
   05 e3 occurs 2.
      10 f3 pic x dynamic length.
      10 m3 pic x(3) value "m3!".
01 g4.
   05 e4 occurs 2.
      10 f4 pic x dynamic length.
      10 m4 pic x(3).
01 i pic 9.
procedure division.
    move 11 to k1(1). move 22 to k1(2). move 33 to k1(3).
    move "hello" to s1.
    move g1 to g2.
    display "g2: " h2 " c2=" c2 " " k2(1) k2(2) k2(3) " s2=[" s2 "] len=" function length(s2) " " z2.
    if g1 = g2 display "g1 = g2" else display "g1 not = g2" end-if.
    move 44 to k2(4).
    if g1 = g2 display "still equal?" else display "g1 not = g2 after k2(4)" end-if.
    if g1 < g2 display "g1 < g2" end-if.
    set c2 to 3.
    if g1 = g2 display "equal again after set c2 to 3" end-if.
    move "hellp" to s2.
    if g1 < g2 display "g1 < g2 by s" end-if.
    move "hello" to s2.
    set c1 down by 3.
    if g1 = g2 display "equal?" else display "empty t1 vs three: not equal" end-if.
    move "one" to f3(1). move "second" to f3(2).
    move g3 to g4.
    display "g4: [" f4(1) "] [" f4(2) "] " m4(2).
    if g3 = g4 display "g3 = g4" end-if.
    stop run.
