identification division.
program-id. p-vlg-shape.
*> Two variable-length groups of different shapes moved whole (2023 8.5.1.12: not compatible).
data division.
working-storage section.
01 g1. 05 t1 pic x(2) occurs dynamic capacity in c1. 05 h1 pic x.
01 g2. 05 h2 pic x. 05 t2 pic x(2) occurs dynamic capacity in c2.
procedure division.
    move g1 to g2
    goback.
