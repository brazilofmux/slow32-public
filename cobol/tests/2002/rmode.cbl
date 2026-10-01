*> ROUNDED MODE (2014; 2023 14.7.4), taken under -std=2002 as BP-E29:
*> the eight modes over positive and negative values, ties and not;
*> PROHIBITED raising the size error and leaving the receiver alone; a
*> mode on ADD, SUBTRACT, MULTIPLY and DIVIDE as well as COMPUTE.  The
*> X-COBOL survey met it (mikebharris' BAMS report).  The oracle runs in
*> GnuCOBOL's default dialect: its -std=cobol2002 has no ROUNDED MODE.
identification division.
program-id. rmode.
data division.
working-storage section.
01 vals.
   05 v   pic s9v999 occurs 6.
01 r      pic s9.
01 r2     pic s9v99.
01 i      pic 9.
procedure division.
    move  2.5   to v(1)
    move -2.5   to v(2)
    move  3.5   to v(3)
    move  2.4   to v(4)
    move -2.6   to v(5)
    move  0.001 to v(6)
    perform varying i from 1 by 1 until i > 6
        display v(i) with no advancing
        compute r rounded mode away-from-zero = v(i)
        display " afz " r with no advancing
        compute r rounded mode nearest-away-from-zero = v(i)
        display " nafz " r with no advancing
        compute r rounded mode nearest-even = v(i)
        display " ne " r with no advancing
        compute r rounded mode nearest-toward-zero = v(i)
        display " ntz " r with no advancing
        compute r rounded mode toward-greater = v(i)
        display " tg " r with no advancing
        compute r rounded mode toward-lesser = v(i)
        display " tl " r with no advancing
        compute r rounded mode truncation = v(i)
        display " tr " r
    end-perform
    move 7 to r
    compute r rounded mode prohibited = 2.5
        on size error display "prohibited: size error, r still " r
        not on size error display "prohibited: stored " r
    end-compute
    compute r rounded mode prohibited = 3.000
        on size error display "prohibited exact: size error"
        not on size error display "prohibited exact: stored " r
    end-compute
    move 1.00 to r2
    add 0.125 to r2 rounded mode nearest-even
    display "add 0.125 nearest-even " r2
    move 1.00 to r2
    add 0.135 to r2 rounded mode nearest-even
    display "add 0.135 nearest-even " r2
    move 1.00 to r2
    subtract 0.125 from r2 rounded mode toward-greater
    display "subtract toward-greater " r2
    move 1.00 to r2
    multiply 1.005 by r2 rounded mode toward-lesser
    display "multiply toward-lesser " r2
    divide 3 into 10 giving r2 rounded mode away-from-zero
    display "divide away-from-zero " r2
    divide -3 into 10 giving r2 rounded mode toward-greater
    display "divide toward-greater " r2
    stop run.
