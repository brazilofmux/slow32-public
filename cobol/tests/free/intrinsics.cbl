*> The 1989 amendment's intrinsic functions (X3.23a): the numeric
*> family through the stack, the string family, NUMVAL both ways,
*> CHAR/ORD as inverses, MAX over strings, nesting, an expression
*> argument, a table's elements as arguments via ALL. RANDOM is
*> checked for range and reproducibility, not value. Oracle: GnuCOBOL.
identification division.
program-id. intrinsics.
data division.
working-storage section.
77  x   pic s9(4)v99 value -12.25.
77  r   pic s9(9)v9(4).
77  i9  pic s9(9).
77  a4  pic x(4).
77  r1  pic 9v9(9).
77  r2  pic 9v9(9).
01  t.
    05  e occurs 4 pic 99.
procedure division.
    move 10 to e (1). move 30 to e (2). move 20 to e (3). move 40 to e (4).
    compute r = function max(3 7 5) display 'max      ' r.
    compute r = function min(3 7 5) display 'min      ' r.
    compute i9 = function ord-max(10 40 20) display 'ord-max  ' i9.
    compute i9 = function ord-min(10 40 20) display 'ord-min  ' i9.
    compute r = function sum(e (1) e (2) e (3) e (4)) display 'sum      ' r.
    compute r = function mean(e (1) e (2) e (3) e (4)) display 'mean     ' r.
    compute r = function median(e (1) e (2) e (3) e (4)) display 'median   ' r.
    compute r = function midrange(e (1) e (2) e (3) e (4)) display 'midrange ' r.
    compute r = function range(e (1) e (2) e (3) e (4)) display 'range    ' r.
    compute r = function variance(2 4 6 8) display 'variance ' r.
    compute r = function standard-deviation(2 4 6 8) display 'stddev   ' r.
    compute i9 = function mod(-11 5) display 'mod      ' i9.
    compute r = function rem(-11 5) display 'rem      ' r.
    compute i9 = function integer(x) display 'integer  ' i9.
    compute i9 = function integer-part(x) display 'intpart  ' i9.
    compute i9 = function factorial(12) display 'fact12   ' i9.
    compute r = function sqrt(2) display 'sqrt2    ' r.
    compute r = function log(10) display 'log10e   ' r.
    compute r = function log10(1000) display 'log10    ' r.
    compute r = function sin(0.5) display 'sin.5    ' r.
    compute r = function cos(0.5) display 'cos.5    ' r.
    compute r = function tan(0.5) display 'tan.5    ' r.
    compute r = function asin(0.5) display 'asin.5   ' r.
    compute r = function acos(0.5) display 'acos.5   ' r.
    compute r = function atan(1) * 4 display 'pi       ' r.
    compute r = function tan(1 / 180) display 'tan-tiny ' r.
    compute r = function annuity(0.01 12) display 'annuity  ' r.
    compute r = function present-value(0.1 100 100) display 'pv       ' r.
    compute i9 = function ord(function char(66)) display 'ordchar  ' i9.
    move function reverse('abcd') to a4 display 'reverse  ' a4.
    move function upper-case(function reverse('dcba')) to a4
    display 'upnest   ' a4.
    move function max('kiwi' 'apple' 'pear') to a4 display 'maxal    ' a4.
    compute r = function numval('  -  4929.0323  ') display 'numval   ' r.
    compute r = function numval-c('- $ 8,90.21') display 'numvalc  ' r.
    display 'wc-len   ' function length(function when-compiled).
    compute r1 = function random(7).
    compute r2 = function random(7).
    if r1 = r2 and r1 >= 0 and r1 < 1 display 'random   seeded-repeatable-in-range'
    else display 'random   BROKEN' end-if.
    compute r = function max(x * -1, (3 + 1) / 2, 3 + 4) display 'maxexpr  ' r.
    stop run.
end program intrinsics.
