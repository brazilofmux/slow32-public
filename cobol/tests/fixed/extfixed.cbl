      * Extended letters in fixed form (2023 8.1.3, Annex B; standard-
      * queue item 47): names and a paragraph with accented letters, a
      * PROGRAM-ID with one, called by a literal of another case (the
      * externalized name folded alike). No oracle: GnuCOBOL 4 refuses.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXTFIXED.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ÉTÉ            PIC X(6) VALUE "soleil".
       01  MONTANT-TOTAL  PIC 9(5) VALUE 12345.
       PROCEDURE DIVISION.
       DÉBUT.
           DISPLAY "été=" ÉTÉ " total=" MONTANT-TOTAL.
           CALL "Übersetzung" USING MONTANT-TOTAL.
           STOP RUN.
       END PROGRAM EXTFIXED.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Übersetzung.
       DATA DIVISION.
       LINKAGE SECTION.
       01  N PIC 9(5).
       PROCEDURE DIVISION USING N.
           DISPLAY "Übersetzung: " N.
       END PROGRAM Übersetzung.
