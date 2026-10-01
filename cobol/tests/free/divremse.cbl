identification division.
program-id. divremse.
*> DIVIDE ... REMAINDER and size errors, X3.23-1985 VI-81 DIVIDE
*> general rules 6-8.  Found by the differential generator
*> (tests/gen): GnuCOBOL stores a remainder after the quotient's size
*> error, which rule 8a forbids (docs/oracles.md).
data division.
working-storage section.
01 q2   pic 9(2).
01 q2r  pic 9(2).
01 rm   pic 9(4).
01 rm1  pic 9.
01 qd   pic 9v9.
01 rmd  pic 9v99.
01 dvd  pic s9(10)v9 usage binary.
01 dvs  pic 9(6)v9(6).
01 qw   pic 9(8)v9(6) usage binary.
01 rmw  pic s9(8)v9(5) usage binary.
procedure division.
*> 8a: size error on the quotient -- both receivers unchanged
    move 11 to q2  move 2222 to rm
    divide 7 into 1000 giving q2 remainder rm
        on size error display "8a size error q2=" q2 " rm=" rm
        not on size error display "8a no size error"
    end-divide
    move 11 to q2r  move 3333 to rm
    divide 7 into 1000 giving q2r rounded remainder rm
        on size error display "8a rounded q2r=" q2r " rm=" rm
    end-divide
*> 8b: size error in the remainder only -- the quotient is stored,
*> the remainder unchanged
    move 11 to q2  move 9 to rm1
    divide 100 into 1234 giving q2 remainder rm1
        on size error display "8b q2=" q2 " rm1=" rm1
        not on size error display "8b no size error q2=" q2 " rm1=" rm1
    end-divide
*> 6: with ROUNDED the remainder uses the truncated quotient:
*> 29 / 8 = 3.625, stored rounded as 4, remainder from the truncated
*> 3: 29 - 24 = 5
    divide 8 into 29 giving q2r rounded remainder rm
    display "6 q2r=" q2r " rm=" rm
*> 6 with decimals: 1 / 0.3 = 3.33.., qd 3.3 (rounded 3.3), remainder
*> 1 - 0.99 = 0.01
    divide 0.3 into 1 giving qd rounded remainder rmd
    display "6 qd=" qd " rmd=" rmd
*> 6, with a product past 18 digits though no item is: the quotient
*> 13653.925354 times the divisor is 22 digits at scale 12; exactly,
*> 8271765550.5 - 13653.925354 x 605815.934675 = 0.183809750050
    move 8271765550.5 to dvd  move 605815.934675 to dvs
    divide dvd by dvs giving qw rounded remainder rmw
    display "6 wide qw=" qw " rmw=" rmw
    stop run.
