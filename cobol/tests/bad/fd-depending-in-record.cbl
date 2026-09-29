       identification division.
       program-id. fddep.
      * RECORD VARYING DEPENDING ON names an unsigned integer outside
      * the file (X3.23-1985 RECORD syntax rule 4).
       environment division.
       input-output section.
       file-control.
           select f assign to "x.dat".
       data division.
       file section.
       fd f record varying in size from 1 to 20 depending on k2.
       01 r.
          05 k2 pic 99.
          05 d pic x(18).
       working-storage section.
       01 n pic 9(4).
       01 sn pic s9(2) value 5.
       procedure division.
           stop run.
