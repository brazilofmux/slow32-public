       identification division.
       program-id. redeff.
      * No REDEFINES on a level 01 entry in the FILE SECTION (X3.23-1985
      * REDEFINES syntax rule 3; 2023 13.18.44.3 rule 3).
       environment division.
       input-output section.
       file-control.
           select f assign to "x.dat".
       data division.
       file section.
       fd f.
       01 r1 pic x(10).
       01 r2 redefines r1 pic x(10).
       working-storage section.
       01 w pic x.
       procedure division.
           stop run.
