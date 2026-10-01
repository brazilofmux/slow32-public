       identification division.
       program-id. s02exb.
      * EXTERNAL and BASED are not in one entry (2002 13.13.2 rule 5;
      * 2023 13.16.3 rule 5).  Accepted before the DATA DIVISION sweep.
       data division.
       working-storage section.
       01 b pic x(4) based external.
       procedure division.
           stop run.
