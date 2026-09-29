       identification division.
       program-id. selrk.
      * RECORD KEY belongs to an indexed file.
       environment division.
       input-output section.
       file-control.
           select f assign to "x.dat" record key k.
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
