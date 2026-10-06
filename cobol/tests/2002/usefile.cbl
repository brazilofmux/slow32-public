      *> USE AFTER EXCEPTION CONDITION exception-name FILE file-name
      *> (2023 14.9.49 format 3): an EC-I-O condition goes to the USE that
      *> names its file before one that names none, level 3 before level
      *> 2 (general rules 3c-g).
      *>   - READ of a missing record in F1: EC-I-O-INVALID-KEY FILE f1
      *>     takes it, not the plain EC-I-O-INVALID-KEY;
      *>   - the same in F2: no USE names f2 at level 3, so EC-I-O FILE f2
      *>     (level 2) takes it, ahead of the plain level-3 one;
      *>   - in F3: only the plain EC-I-O-INVALID-KEY applies.
      *> No oracle: GnuCOBOL 4 does not implement exception declaratives
      *> or the exception-checking PERFORM (docs/standards.md).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. usefile.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT f1 ASSIGN TO "usefile1.dat" ORGANIZATION INDEXED
               ACCESS RANDOM RECORD KEY k1 FILE STATUS s1.
           SELECT f2 ASSIGN TO "usefile2.dat" ORGANIZATION INDEXED
               ACCESS RANDOM RECORD KEY k2 FILE STATUS s2.
           SELECT f3 ASSIGN TO "usefile3.dat" ORGANIZATION INDEXED
               ACCESS RANDOM RECORD KEY k3 FILE STATUS s3.
       DATA DIVISION.
       FILE SECTION.
       FD f1. 01 r1. 05 k1 PIC X(4). 05 d1 PIC X(4).
       FD f2. 01 r2. 05 k2 PIC X(4). 05 d2 PIC X(4).
       FD f3. 01 r3. 05 k3 PIC X(4). 05 d3 PIC X(4).
       WORKING-STORAGE SECTION.
       01 s1 PIC XX. 01 s2 PIC XX. 01 s3 PIC XX.
       PROCEDURE DIVISION.
       DECLARATIVES.
       plain SECTION.
           USE AFTER EXCEPTION CONDITION EC-I-O-INVALID-KEY.
           DISPLAY "plain invalid-key".
       for-f1 SECTION.
           USE AFTER EC EC-I-O-INVALID-KEY FILE f1.
           DISPLAY "invalid-key, file f1".
       for-f2 SECTION.
           USE AFTER EC EC-I-O FILE f2.
           DISPLAY "any I-O condition, file f2".
       END DECLARATIVES.
       main SECTION.
       >>TURN EC-I-O CHECKING ON
           OPEN OUTPUT f1 f2 f3 CLOSE f1 f2 f3
           OPEN INPUT f1 f2 f3
           MOVE "none" TO k1 READ f1
           MOVE "none" TO k2 READ f2
           MOVE "none" TO k3 READ f3
           CLOSE f1 f2 f3
           DISPLAY "done " s1 " " s2 " " s3
           STOP RUN.
