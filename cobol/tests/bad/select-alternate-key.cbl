       identification division.
       program-id. selak.
      * An ALTERNATE RECORD KEY does not begin where the RECORD KEY does
      * (X3.23-1985 ALTERNATE RECORD KEY syntax rule 4).
       environment division.
       input-output section.
       file-control.
           select f assign to "x.dat" organization indexed record key k
               alternate record key k.
       data division.
       file section.
       fd f.
       01 r.
          05 k pic x(4).
          05 st pic xx.
          05 d pic x(10).
       working-storage section.
       01 n pic 9(4).
       01 sn pic s9(2) value 5.
       procedure division.
           stop run.
