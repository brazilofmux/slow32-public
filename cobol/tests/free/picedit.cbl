*> PICTURE editing rules at their edges (X3.23-1985 VI-38..43; 2023
*> 13.18.40.5): Z in every digit position blanks a zero value entirely,
*> the point too; * keeps the point; a floating string past the point;
*> CR and DB; fixed + and - at either end; P scaling; BLANK WHEN ZERO.
*> docs/conformance/picture.md
identification division.
program-id. picedit.
data division.
working-storage section.
01 v   pic s9(4)v99.
01 e1  pic zzzz.zz.
01 e2  pic ****.**.
01 e3  pic $$$$.$$.
01 e4  pic ++++.++.
01 e5  pic zzz9.99cr.
01 e6  pic zzz9.99db.
01 e7  pic -zzz9.99.
01 e8  pic zzz9.99+.
01 e9  pic $zz9.99-.
01 e10 pic zzz9.99 blank when zero.
01 e11 pic 99pp.
01 e12 pic $**,**9.99.
01 e13 pic -(5)9.
01 e14 pic 99b99/99.
01 k   pic 9.
procedure division.
    perform varying k from 1 by 1 until k > 4
        evaluate k
            when 1 move 0 to v
            when 2 move 0.05 to v
            when 3 move -1.5 to v
            when 4 move 1234.56 to v
        end-evaluate
        move v to e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e12 e13 e14
        display "[" e1 "][" e2 "][" e3 "][" e4 "][" e5 "]"
        display "[" e6 "][" e7 "][" e8 "][" e9 "][" e10 "]"
        display "[" e12 "][" e13 "][" e14 "]"
    end-perform
    move 1234 to e11
    display "[" e11 "]"
    stop run.
