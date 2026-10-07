identification division.
program-id. numvalanycase.
*> NUMVAL-C and TEST-NUMVAL-C with argument-2 ANYCASE (2023 15.68,
*> 15.94; COBOL 2014; standard-queue item 14): the currency string
*> matched in either case; without the phrase, as written.  No oracle:
*> GnuCOBOL 4 does not support ANYCASE.
data division.
working-storage section.
01  n        pic 9(5)v99.
01  t        pic 9(3).
procedure division.
    compute n = function numval-c("EUR12.50" "EUR") display "numval-c   " n
    compute n = function numval-c("eur 7.25" "EUR" anycase) display "anycase    " n
    compute n = function numval-c("Eur 7.25" "eur" anycase) display "anycase 2  " n
    move function test-numval-c("Eur 3" "EUR" anycase) to t display "test   " t
    move function test-numval-c("Eur 3" "EUR") to t display "no anycase " t
    stop run.
end program numvalanycase.
