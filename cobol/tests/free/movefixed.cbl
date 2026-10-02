IDENTIFICATION DIVISION.
PROGRAM-ID. MOVEFIXED.
*> Alphanumeric moves whose lengths the compiler can count are copies of
*> the receiver's length, not calls of the runtime's MOVE: a group to an
*> item no longer than it, and a reference-modified part to or from an
*> alphanumeric item or another part, the sender as long as the receiver
*> or longer (cobol ISSUES-122).  Beside them, the same moves where the
*> rule does not hold -- a shorter sender (padded), a JUSTIFIED receiver,
*> a numeric receiver, a computed length -- which still go to the runtime.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  REC.
    05  R-NUM    PIC 9(10) VALUE 1234567890.
    05  R-NAME   PIC X(12) VALUE "ALPHA-BRAVO-".
    05  R-AMT    PIC S9(5)V99 VALUE -123.45.
01  X4       PIC X(4).
01  X8       PIC X(8).
01  X29      PIC X(29).
01  X40      PIC X(40).
01  J6       PIC X(6) JUSTIFIED RIGHT.
01  N4       PIC 9(4).
01  G2.
    05  G2A  PIC X(3) VALUE "abc".
    05  G2B  PIC 9(3) VALUE 7.
01  T.
    05  TE   PIC X(5) OCCURS 4.
01  I        PIC 9 VALUE 3.
01  L        PIC 9 VALUE 2.
PROCEDURE DIVISION.
MAIN.
    MOVE R-NUM(7:4) TO X4                 DISPLAY "part to item, equal: [" X4 "]"
    MOVE R-NAME(1:8) TO X4                DISPLAY "part to item, cut: [" X4 "]"
    MOVE R-NAME(7:2) TO X4                DISPLAY "part to item, padded: [" X4 "]"
    MOVE SPACES TO X8
    MOVE R-NAME(3:4) TO X8(2:4)           DISPLAY "part to part, equal: [" X8 "]"
    MOVE R-NAME(1:6) TO X8(6:3)           DISPLAY "part to part, cut: [" X8 "]"
    MOVE "ZZZZZZZZ" TO X8
    MOVE R-NAME(1:2) TO X8(3:5)           DISPLAY "part to part, padded: [" X8 "]"
    MOVE R-NAME TO X8(1:8)                DISPLAY "item to part, cut: [" X8 "]"
    MOVE X4 TO X8(5:4)                    DISPLAY "item to part, equal: [" X8 "]"
    MOVE R-NAME(I:4) TO X4                DISPLAY "computed start: [" X4 "]"
    MOVE R-NAME(I:L) TO X4                DISPLAY "computed length: [" X4 "]"
    MOVE R-AMT(1:7) TO X8(1:7)            DISPLAY "a signed item's part: [" X8(1:7) "]"
    MOVE R-NAME(1:6) TO J6                DISPLAY "to a justified item: [" J6 "]"
    MOVE R-NAME(1:8) TO J6                DISPLAY "to a justified item, cut: [" J6 "]"
    MOVE R-NUM(3:4) TO N4                 DISPLAY "part to a numeric item: " N4
    MOVE REC TO X29                       DISPLAY "group to item, equal: [" X29 "]"
    MOVE REC TO X8                        DISPLAY "group to item, cut: [" X8 "]"
    MOVE REC TO X40                       DISPLAY "group to item, padded: [" X40 "]"
    MOVE G2 TO J6                         DISPLAY "group to a justified item: [" J6 "]"
    MOVE G2 TO N4                         DISPLAY "group to a numeric item: [" N4(1:4) "]"
    MOVE "12345" TO TE(1) TE(2) TE(3) TE(4)
    MOVE R-NAME(1:5) TO TE(I)             DISPLAY "part to an element: [" T "]"
    MOVE TE(I)(2:3) TO TE(I + 1)(1:3)     DISPLAY "element part to element part: [" T "]"
    STOP RUN.
