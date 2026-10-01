       identification division.
       program-id. glbsra.
      * Files in a SAME RECORD AREA, and their records, are not GLOBAL
      * (X3.23-1985 X-24, GLOBAL syntax rule 3; 2023 13.18.27.3 rule
      * 2).  Accepted before the DATA DIVISION sweep (2026-09-30).
       environment division.
       input-output section.
       file-control.
           select f1 assign to "x.dat".
           select f2 assign to "y.dat".
       i-o-control.
           same record area for f1 f2.
       data division.
       file section.
       fd f1 global.
       01 r1 pic x(10).
       fd f2.
       01 r2 pic x(10).
       procedure division.
           stop run.
