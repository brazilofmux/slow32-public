       identification division.
       program-id. entrul.
      * The data description entry (X3.23-1985 VI-18, VI-21; 2023
      * 13.16.3): REDEFINES comes right after the name (syntax rule 2);
      * BLANK WHEN ZERO is for an elementary item (general rule 1); a
      * level 77 entry has a data-name (VI-18).  All three were accepted
      * before the DATA DIVISION sweep (2026-09-30).
       data division.
       working-storage section.
       01 b pic x(4).
       01 a pic x(4) redefines b.
       01 g blank when zero.
          05 n pic 9.
       77 pic x.
       procedure division.
           stop run.
