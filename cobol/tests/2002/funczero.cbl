*> A numeric function's result beside ZERO is compared as the number 0
*> (2023 8.8.4.2.5: ZERO is a numeric literal opposite a numeric
*> operand), and a sign condition takes one (8.8.4.6).  ZERO was filled
*> to the function's size as characters, so IF FUNCTION TEST-DATE-YYYYMMDD
*> (d) NOT = ZERO held for every date (ACAS's maps04, cobol ISSUES-124).
 identification division.
 program-id. funczero.
 data division.
 working-storage section.
 01  p pic 9(8) value 20260401.
 procedure division.
     if function test-date-yyyymmdd (p) = zero display "1 = zero: yes" else display "1 = zero: NO" end-if
     if function test-date-yyyymmdd (p) = 0 display "2 = 0: yes" else display "2 = 0: NO" end-if
     if function test-date-yyyymmdd (p) not = 0 display "3 not = 0: WRONG" else display "3 not = 0: ok" end-if
     if function integer (0) = zero display "4 integer(0) = zero: yes" else display "4 integer(0) = zero: NO" end-if
     if function abs (0) = zero display "5 abs(0) = zero: yes" else display "5 abs(0) = zero: NO" end-if
     if function test-date-yyyymmdd (p) = function integer (0) display "6 = integer(0): yes" else display "6: NO" end-if
     if function test-date-yyyymmdd (p) < 1 display "8 < 1: yes" else display "8 < 1: NO" end-if
     if function test-date-yyyymmdd (p) is zero display "9 is zero: yes" else display "9 is zero: NO" end-if
     if function integer-of-date (p) is positive display "10 positive: yes" else display "10 positive: NO" end-if
     if function upper-case ("a") = space display "11 WRONG" else display "11 alnum vs space: ok" end-if
     stop run.
