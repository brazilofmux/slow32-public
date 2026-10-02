IDENTIFICATION DIVISION.
PROGRAM-ID. EVALFUNC.
*> An EVALUATE subject that is an alphanumeric or national function is
*> evaluated once (2023 14.9.13.4 rule 3): its result is kept, with the
*> length a result of run-time length has, and each WHEN compares against
*> that -- whatever functions the WHEN's own objects evaluate in between
*> (cob_fn_keep, cob_fn_kept; cobol ISSUES-121).
DATA DIVISION.
WORKING-STORAGE SECTION.
01  S        PIC X(10) VALUE "  delta   ".
01  T        PIC X(10) VALUE "DELTA".
01  E        PIC X(4)  VALUE SPACES.
01  N        PIC N(4)  VALUE N"abcd".
PROCEDURE DIVISION.
MAIN.
    EVALUATE FUNCTION TRIM(S)
        WHEN FUNCTION TRIM("  alpha ") DISPLAY "trim: alpha"
        WHEN FUNCTION LOWER-CASE(FUNCTION TRIM(T)) DISPLAY "trim: delta"
        WHEN OTHER DISPLAY "trim: other"
    END-EVALUATE
    *> the first WHEN's object is a shorter result: the subject's own
    *> length must be back for the second
    EVALUATE FUNCTION TRIM(S)
        WHEN FUNCTION TRIM(" de ") DISPLAY "length: de"
        WHEN "delta" DISPLAY "length: delta"
        WHEN OTHER DISPLAY "length: other"
    END-EVALUATE
    EVALUATE FUNCTION UPPER-CASE(S)
        WHEN "  ALPHA" DISPLAY "upper: alpha"
        WHEN FUNCTION UPPER-CASE("  delta") DISPLAY "upper: delta"
        WHEN OTHER DISPLAY "upper: other"
    END-EVALUATE
    EVALUATE FUNCTION TRIM(S)
        WHEN "a" THRU "c" DISPLAY "range: a to c"
        WHEN FUNCTION TRIM(" d ") THRU FUNCTION TRIM(" e ") DISPLAY "range: d to e"
        WHEN OTHER DISPLAY "range: other"
    END-EVALUATE
    EVALUATE FUNCTION UPPER-CASE(S)(3:5) ALSO FUNCTION LENGTH(FUNCTION TRIM(S))
        WHEN "DELTA" ALSO 4 DISPLAY "part: DELTA, 4"
        WHEN FUNCTION UPPER-CASE("delta") ALSO 5 DISPLAY "part: DELTA, 5"
        WHEN OTHER DISPLAY "part: other"
    END-EVALUATE
    EVALUATE FUNCTION UPPER-CASE(N)
        WHEN FUNCTION LOWER-CASE(N) DISPLAY "national: lower"
        WHEN N"ABCD" DISPLAY "national: ABCD"
        WHEN OTHER DISPLAY "national: other"
    END-EVALUATE
    EVALUATE FUNCTION REVERSE(T)
        WHEN FUNCTION REVERSE("DELTA") DISPLAY "reverse: of a literal"
        WHEN "     ATLED" DISPLAY "reverse: ATLED after five spaces"
        WHEN OTHER DISPLAY "reverse: other"
    END-EVALUATE
    STOP RUN.
