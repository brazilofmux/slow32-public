      *> WHEN EXCEPTION file-name / open mode in an exception-checking
      *> PERFORM (2023 14.9.28 format 3): a file's I-O error goes to the
      *> WHEN naming the file, else the WHEN naming its open mode, as a
      *> USE AFTER EXCEPTION PROCEDURE would choose (14.9.49.4 rules 3a-b),
      *> and the USE procedure that would have run does not (rule 17).
      *> Execution resumes after the failing statement (rule 20).
      *> No oracle: GnuCOBOL 4 does not implement exception declaratives
      *> or the exception-checking PERFORM (docs/standards.md).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ecpfile.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT f1 ASSIGN TO "ecpfile1.dat" ORGANIZATION INDEXED
               ACCESS RANDOM RECORD KEY k1.
           SELECT f2 ASSIGN TO "ecpfile2.dat" ORGANIZATION INDEXED
               ACCESS RANDOM RECORD KEY k2.
       DATA DIVISION.
       FILE SECTION.
       FD f1. 01 r1. 05 k1 PIC X(4). 05 d1 PIC X(4).
       FD f2. 01 r2. 05 k2 PIC X(4). 05 d2 PIC X(4).
       PROCEDURE DIVISION.
       DECLARATIVES.
       errs SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON f1 f2.
           DISPLAY "the USE procedure (should not run)".
       END DECLARATIVES.
       main SECTION.
           OPEN OUTPUT f1 f2 CLOSE f1 f2
           OPEN INPUT f1 f2
           PERFORM
               MOVE "none" TO k1 READ f1
               DISPLAY "after the read of f1"
               MOVE "none" TO k2 READ f2
               DISPLAY "after the read of f2"
           WHEN EXCEPTION f1
               DISPLAY "when f1"
           WHEN EXCEPTION INPUT
               DISPLAY "when input"
           END-PERFORM
           CLOSE f1 f2
           DISPLAY "done"
           STOP RUN.
