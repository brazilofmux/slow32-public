identification division.
program-id. fnargbad.
*> An argument outside a function's rules, EC-ARGUMENT-FUNCTION not
*> checked: the result is the implementor's (2023 15.3), and here it is
*> 0, as GnuCOBOL's is.  MOD and REM by zero stopped the run, and the
*> functions computed in double gave NaN's or saturated values.  Checked,
*> the condition is raised: tests/ecsites/argfn.txt, one program a site.
data division.
working-storage section.
01 m1  pic s9 value -1.
01 z0  pic 9 value 0.
01 p2  pic 9 value 2.
01 p7  pic 9 value 7.
01 p300 pic 999 value 300.
01 bad pic 9(8) value 20231301.
01 r   pic -(9)9.9(6).
procedure division.
    compute r = function sqrt(m1)             display "sqrt(-1)      " r
    compute r = function log(z0)              display "log(0)        " r
    compute r = function mod(p7, z0)          display "mod(7, 0)     " r
    compute r = function rem(p7, z0)          display "rem(7, 0)     " r
    compute r = function asin(p2)             display "asin(2)       " r
    compute r = function factorial(m1)       display "factorial(-1) " r
    compute r = function integer-of-date(bad) display "int-of-date   " r
    compute r = function date-of-integer(z0)  display "date-of-int(0)" r
    compute r = function annuity(m1, p7)      display "annuity(-1, 7)" r
    compute r = function exp(p300)            display "exp(300)      " r
    stop run.
end program fnargbad.
