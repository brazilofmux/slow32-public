       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQST.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           ALPHABET NATIVE-SET IS STANDARD-1
           ALPHABET BACKWARDS IS "z" THRU "a" "Z" THRU "A".
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATA-FILE ASSIGN TO 'sqst-data.dat'
               ORGANIZATION SEQUENTIAL.
           SELECT PRINT-FILE ASSIGN TO 'sqst-print.txt'
               ORGANIZATION LINE SEQUENTIAL.
           SELECT WORK-FILE ASSIGN TO 'sqst-work.tmp'.
       DATA DIVISION.
       FILE SECTION.
       FD  DATA-FILE CODE-SET IS NATIVE-SET.
       01  DATA-REC        PIC X(8).
       FD  PRINT-FILE.
       01  PRINT-REC       PIC X(20).
       SD  WORK-FILE.
       01  WORK-REC.
           05  W-KEY       PIC X(3).
           05  W-REST      PIC X(5).
       WORKING-STORAGE SECTION.
       01  I               PIC 9.
       01  NAMES.
           05  FILLER      PIC X(8) VALUE 'ant   01'.
           05  FILLER      PIC X(8) VALUE 'Bee   02'.
           05  FILLER      PIC X(8) VALUE 'cat   03'.
           05  FILLER      PIC X(8) VALUE 'Dog   04'.
       01  NAME-TBL REDEFINES NAMES.
           05  NAME-ENTRY  PIC X(8) OCCURS 4 TIMES.
       PROCEDURE DIVISION.
       MAIN.
           OPEN OUTPUT DATA-FILE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               MOVE NAME-ENTRY (I) TO DATA-REC
               WRITE DATA-REC
           END-PERFORM.
           CLOSE DATA-FILE.
           OPEN INPUT DATA-FILE REVERSED.
           PERFORM 5 TIMES
               READ DATA-FILE AT END DISPLAY 'reversed: at end'
                   NOT AT END DISPLAY 'reversed: ' DATA-REC
               END-READ
           END-PERFORM.
           CLOSE DATA-FILE.
           SORT WORK-FILE ON ASCENDING KEY W-KEY
               SEQUENCE BACKWARDS
               USING DATA-FILE
               OUTPUT PROCEDURE SHOW-SORTED.
           OPEN OUTPUT PRINT-FILE.
           MOVE 'line one' TO PRINT-REC.
           WRITE PRINT-REC IN PRINT-FILE BEFORE ADVANCING ZERO.
           MOVE 'line two' TO PRINT-REC.
           WRITE PRINT-REC AFTER ADVANCING 1 LINE.
           CLOSE PRINT-FILE.
           OPEN INPUT PRINT-FILE.
           PERFORM 3 TIMES
               READ PRINT-FILE AT END DISPLAY 'print: at end'
                   NOT AT END DISPLAY 'print: [' PRINT-REC ']'
               END-READ
           END-PERFORM.
           CLOSE PRINT-FILE.
           STOP RUN.
       SHOW-SORTED.
           PERFORM 5 TIMES
               RETURN WORK-FILE AT END DISPLAY 'sorted: at end'
                   NOT AT END DISPLAY 'sorted: ' WORK-REC
               END-RETURN
           END-PERFORM.
