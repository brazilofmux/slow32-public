*> Arithmetic expressions (X3.23-1985 6.2.3; 2023 8.8.1.2): precedence,
*> left to right within a level -- exponentiation included --, unary
*> minus first, and exponentiation's rules: the positive root, zero
*> to a power not above zero and a negative base to a fraction a size
*> error.  GnuCOBOL departs twice (.oracle-expected, docs/oracles.md).
identification division.
program-id. arithexpr.
data division.
working-storage section.
01 r   pic s9(9)v9(4).
01 z   pic 9 value 0.
01 m   pic s9 value -8.
procedure division.
    compute r = 2 ** 3 ** 2 display "2**3**2 " r
    compute r = - 2 ** 2 display "-2**2 " r
    compute r = 2 * 3 ** 2 display "2*3**2 " r
    compute r = 8 / 4 / 2 display "8/4/2 " r
    compute r = 10 - 4 - 3 display "10-4-3 " r
    compute r = 4 ** 0.5 display "4**0.5 " r
    compute r = 2 ** -1 display "2**-1 " r
    move 7 to r
    compute r = z ** 0 on size error display "0**0 size error" not on size error display "0**0 " r end-compute
    move 7 to r
    compute r = z ** -1 on size error display "0**-1 size error" not on size error display "0**-1 " r end-compute
    move 7 to r
    compute r = m ** 0.5 on size error display "-8**0.5 size error" not on size error display "-8**0.5 " r end-compute
    compute r = m ** 2 display "-8**2 " r
    compute r = m ** 3 display "-8**3 " r
    stop run.
