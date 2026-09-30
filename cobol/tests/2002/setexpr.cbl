identification division.
program-id. setexpr.
*> SET with arithmetic expressions (COBOL 2002 14.8.35, format 1's
*> arithmetic-expression-1 and format 2's arithmetic-expression-2; 2023
*> 14.9.39): the value is taken once, at the start of the statement,
*> then given to each index in turn.  A value that is not an integer
*> is EC-BOUND-SUBSCRIPT, and the indexes are left as they were (2023
*> 14.9.39.4 rules 2-3); unchecked, the fraction is dropped.  No oracle:
*> GnuCOBOL takes an identifier or an integer only (the 1985 formats).
data division.
working-storage section.
01  t1.
    05 a     pic x(3) occurs 9 indexed by i1 j1.
01  t2.
    05 b     pic x(2) occurs 4 indexed by i2.
01  n        pic 99.
01  m        pic 99.
01  nd       pic 9v9.
procedure division.
declaratives.
bd section.
    use after exception condition ec-bound-subscript.
b1.
    set n to i1  set m to j1
    display "declarative: " function exception-status
            " i1=" n " j1=" m.
end declaratives.
main section.
m1.
    move "aaabbbcccdddeeefffggghhhiii" to t1
    move 3 to n
    set i1 to n + 1                 display "n + 1:        " a(i1)
    set i1 up by n * 2 - 3          display "up n*2-3:     " a(i1)
    set i1 down by (n - 1)          display "down (n-1):   " a(i1)
    set i1 j1 to -1 + n * 3         display "both to 3n-1: " a(i1) " " a(j1)
    set i2 to 2
    set i1 to i2 + 1                display "i2 + 1:       " a(i1)
*> each index takes the value as it was at the start
    set j1 to 2  set i1 to 5
    set i1 j1 up by function max(n, 4)
    display "up max(n,4):  " a(i1) " " a(j1)
*> a decimal item is an arithmetic expression too; unchecked, 2.5 is 2
    move 2.5 to nd
    set i1 to nd                    display "nd=2.5, unchecked: " a(i1)
    move 2.0 to nd
    set i1 to nd * 3                display "nd*3 (6.0):  " a(i1)
>>TURN EC-BOUND-SUBSCRIPT CHECKING ON
    set i1 to 4  set j1 to 7
    move 2.5 to nd
    set i1 j1 to nd * 1
    display "not reached"
    stop run.
end program setexpr.
