*> ARITHMETIC IS STANDARD-DECIMAL (COBOL 2014; 2023 8.8.1.5, 11.9.5 GR 3,
*> 11.9.11 GR 3; docs/wide.md): the unit's arithmetic is decimal128's --
*> every intermediate an SDIDI of 34 significant digits, rounded by the
*> INTERMEDIATE ROUNDING mode (NEAREST-AWAY-FROM-ZERO implied), exponents
*> to 6144.  Where NATIVE and STANDARD-DECIMAL part: 2 / 3 * 3 is 2, not
*> 1.99; 1E40 is an intermediate, not a size error; a float operand is
*> converted exactly (0.1 as a double is 0.1000000000000000055511151231257827
*> to 34 digits, so (d - 0.1) is its binary residue, where NATIVE's double
*> arithmetic gives 0); a function's value is an SDIDI (MEAN(1 2 2) * 3 is
*> 5, not 4.99); a comparison compares SDIDIs; exponentiation follows
*> 8.8.1.5.4 (2 ** -2 is 1 / (2 ** 2)); an inexact intermediate under
*> PROHIBITED is the size error condition (EC-SIZE-TRUNCATION), past
*> 9.99E6144 EC-SIZE-OVERFLOW, fatal when checked, so it comes last; a contained program inherits the clause (11.9.4) and
*> may say NATIVE.  No oracle: GnuCOBOL marks the clause "not implemented",
*> gcobol computes natively; the witness is Python's decimal module at
*> precision 34 (decimal128), and gen/stddec runs it over random programs.
identification division.
program-id. stddec.
options.
    arithmetic is standard-decimal.
data division.
working-storage section.
01 s pic s9(3)v9(18).
01 w pic 9v9(30).
01 big pic 9(31).
01 d usage comp-2 value 0.1.
01 f pic 9(5)v9(14).
01 t pic 9(3)v99.
procedure division.
declaratives.
d1 section.
    use after exception condition ec-size-overflow.
d1p.
    display "  EC-SIZE-OVERFLOW".
end declaratives.
main section.
m1.
    compute s = 2 / 3 * 3 display "2/3*3         " s
    compute s = 1 / 3 * 3 display "1/3*3         " s
    compute s = 1 / 7 * 7 display "1/7*7         " s
    compute w = 2 / 3 display "2/3           " w
    compute w rounded = 2 / 3 display "2/3 rounded   " w
    compute big = 10 ** 20 * 10 ** 20 / 10 ** 10 display "1E40/1E10     " big
    compute s = function mean(1 2 2) * 3 display "mean*3        " s
    compute f = (d - 0.1) * 1000000000000000000000 display "(d-0.1)*1E21  " f
    if 2 / 3 * 3 = 2 display "2/3*3 = 2 is true" else display "2/3*3 = 2 is false" end-if
    if d = 0.1 display "d = 0.1 is true" else display "d = 0.1 is false (the double is not one tenth)" end-if
    compute s = 0.5 ** 4 display "0.5**4        " s
    compute s = 2 ** -2 display "2**-2         " s
    compute s = 1.1 ** 3 display "1.1**3        " s
    compute big = 10 ** 30 * 10 ** 30 / 10 ** 30 display "1E60/1E30     " big
    compute t = (10 ** 1000) ** 1000
        on size error display "1E1000000: size error"
    end-compute
    call "sdeven"
    call "sdtrunc"
    call "sdprohib"
    call "sdnative"
    compute s = 2 / 3 * 3 display "outer again   " s
    display "checked overflow last: EC-SIZE-OVERFLOW is fatal, the run unit ends after the declarative"
    >>turn ec-size-overflow checking on
    compute t = (10 ** 1000) ** 1000
    display "not reached: EC-SIZE-OVERFLOW is fatal"
    stop run.
identification division.
program-id. sdeven.
options.
    intermediate rounding is nearest-even.
data division.
working-storage section.
01 w pic 9v9(30).
01 s pic s9(3)v9(18).
procedure division.
    display "nearest-even (the clause inherited: standard-decimal)"
    compute w = 2 / 3 display "  2/3         " w
    compute s = 0.125 * 10 ** 32 / 10 ** 33 display "  0.0125      " s
    compute s = 2 / 3 * 3 display "  2/3*3       " s
    exit program.
end program sdeven.
identification division.
program-id. sdtrunc.
options.
    intermediate rounding is truncation.
data division.
working-storage section.
01 s pic s9(3)v9(18).
procedure division.
    display "truncation"
    compute s = 2 / 3 * 3 display "  2/3*3       " s
    exit program.
end program sdtrunc.
identification division.
program-id. sdprohib.
options.
    intermediate rounding is prohibited.
data division.
working-storage section.
01 s pic s9(3)v9(18).
procedure division.
    display "prohibited"
    compute s = 0.625 * 2 display "  0.625*2     " s
    compute s = 2 / 3 * 3
        on size error display "  2/3*3 size error"
    end-compute
    compute s = 1 / 8 * 8 display "  1/8*8       " s
    compute s = 2 / 3 display "  2/3 unchecked, no phrase: s unchanged " s
    exit program.
end program sdprohib.
identification division.
program-id. sdnative.
options.
    arithmetic is native.
data division.
working-storage section.
01 s pic s9(3)v9(18).
procedure division.
    display "native again, by its own clause"
    compute s = 2 / 3 * 3 display "  2/3*3       " s
    exit program.
end program sdnative.
end program stddec.
