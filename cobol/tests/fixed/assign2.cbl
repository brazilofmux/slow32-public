       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGN2.
      * Two ASSIGN shapes of the Open Systems suite (GitHub #34):
      * RM/COBOL's device word before the name (RANDOM, PRINT), which
      * says nothing here and is ignored, and a group item as the
      * file-name -- "ASG2B." plus a module suffix; and UNLOCK, RM's
      * record locking released, a no-op here. Default dialect.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F1 ASSIGN TO RANDOM "ASG2A.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT F2 ASSIGN TO TABLE-FILE
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  F1.
       01  R1 PIC X(20).
       FD  F2.
       01  R2 PIC X(20).
       WORKING-STORAGE SECTION.
       01  TABLE-FILE.
           05  FILLER     PIC X(6) VALUE "ASG2B.".
           05  APPL-NAME  PIC XX   VALUE SPACE.
       01  W PIC X(20).
       PROCEDURE DIVISION.
       MAIN.
           MOVE "GL" TO APPL-NAME.
           OPEN OUTPUT F1. MOVE "FIRST" TO R1. WRITE R1.
           UNLOCK F1 RECORD. CLOSE F1.
           OPEN OUTPUT F2. MOVE "SECOND" TO R2. WRITE R2. CLOSE F2.
           OPEN INPUT F1. READ F1 INTO W. CLOSE F1. DISPLAY W.
           OPEN INPUT F2. READ F2 INTO W. CLOSE F2. DISPLAY W.
           STOP RUN.
