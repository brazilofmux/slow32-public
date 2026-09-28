identification division.
program-id. intr2002.
*> The COBOL 2002 intrinsic functions beyond the 1989 set (2002 15.x;
*> cobol ISSUES-52): numeric (ABS, EXP, EXP10, PI, SIGN, FRACTION-PART,
*> HIGHEST- and LOWEST-ALGEBRAIC), the date windows (YEAR-TO-YYYY,
*> DATE-TO-YYYYMMDD, DAY-TO-YYYYDDD, TEST-DATE-YYYYMMDD,
*> TEST-DAY-YYYYDDD), BYTE-LENGTH, NUMVAL-F and the TEST-NUMVAL family.
*> The clock is fixed (intr2002.env) for the default window year.
*> GnuCOBOL 4 agrees on all but TEST-NUMVAL-C with a currency string:
*> "EUR 12.50" with "EUR" conforms (15.55.2 rule 5; its own NUMVAL-C
*> converts it), and GnuCOBOL reports position 2 (intr2002.oracle-expected).
data division.
working-storage section.
01  n        pic s9(5)v9(4).
01  i        pic s9(9).
01  e        pic 9(9)v9(6).
01  r1       pic s999.
01  r2       pic 99v9(3).
01  r3       pic s9(4) binary.
01  s        pic x(12).
01  k        pic 99.
01  strs.
    05 filler pic x(12) value "123".
    05 filler pic x(12) value " -1.5 ".
    05 filler pic x(12) value "1.5-".
    05 filler pic x(12) value "0 1".
    05 filler pic x(12) value "abc".
    05 filler pic x(12) value "1.2.3".
    05 filler pic x(12) value "+1-".
    05 filler pic x(12) value "  12 CR".
    05 filler pic x(12) value "1,234".
    05 filler pic x(12) value spaces.
01  strtab redefines strs.
    05 str   pic x(12) occurs 10.
procedure division.
main.
    move function abs(-12.5) to n        display "abs(-12.5) = " n
    move function sign(-3) to i          display "sign(-3) = " i
    move function sign(0) to i           display "sign(0) = " i
    move function fraction-part(-1.5) to n display "fraction-part(-1.5) = " n
    move function pi to n                display "pi = " n
    move function exp(1) to e            display "exp(1) = " e
    move function exp10(3) to e          display "exp10(3) = " e
    move function highest-algebraic(r1) to n display "highest(s999) = " n
    move function lowest-algebraic(r1) to n  display "lowest(s999) = " n
    move function highest-algebraic(r2) to n display "highest(99v999) = " n
    move function lowest-algebraic(r2) to n  display "lowest(99v999) = " n
    move function highest-algebraic(r3) to n display "highest(s9(4) binary) = " n
    move function byte-length(r3) to i   display "byte-length(s9(4) binary) = " i
    move function byte-length(s) to i    display "byte-length(x(12)) = " i
    move function year-to-yyyy(4, 23, 1995) to i display "year-to-yyyy(4, 23, 1995) = " i
    move function year-to-yyyy(98, -15, 2008) to i display "year-to-yyyy(98, -15, 2008) = " i
    move function year-to-yyyy(30) to i  display "year-to-yyyy(30) in 2026 = " i
    move function year-to-yyyy(90) to i  display "year-to-yyyy(90) in 2026 = " i
    move function date-to-yyyymmdd(851003, 10, 2002) to i display "date-to-yyyymmdd(851003, 10, 2002) = " i
    move function day-to-yyyyddd(10004, 20, 2002) to i display "day-to-yyyyddd(10004, 20, 2002) = " i
    move function test-date-yyyymmdd(20260229) to i display "test-date(20260229) = " i
    move function test-date-yyyymmdd(20240229) to i display "test-date(20240229) = " i
    move function test-date-yyyymmdd(20241301) to i display "test-date(20241301) = " i
    move function test-date-yyyymmdd(15001231) to i display "test-date(15001231) = " i
    move function test-day-yyyyddd(2024366) to i display "test-day(2024366) = " i
    move function test-day-yyyyddd(2023366) to i display "test-day(2023366) = " i
    move function numval-f("1.5E+3") to n  display "numval-f(1.5E+3) = " n
    move function numval-f(" -2.5e-2 ") to n display "numval-f(-2.5e-2) = " n
    perform varying k from 1 by 1 until k > 10
        move function test-numval(str(k)) to i
        display "test-numval([" str(k) "]) = " i
    end-perform
    move function test-numval-c("$1,234.50CR") to i display "test-numval-c($1,234.50CR) = " i
    move function test-numval-c("EUR 12.50", "EUR") to i display "test-numval-c(EUR 12.50, EUR) = " i
    move function numval-c("EUR 12.50", "EUR") to n display "numval-c(EUR 12.50, EUR) = " n
    move function test-numval-f("1.5E+3") to i display "test-numval-f(1.5E+3) = " i
    move function test-numval-f("1.5E3") to i display "test-numval-f(1.5E3) = " i
    move function test-numval-f("1.5E+") to i display "test-numval-f(1.5E+) = " i
    stop run.
end program intr2002.
