       identification division.
       program-id. ua.
      * The USE statement is a sentence by itself (2023 14.9.49.3
      * rule 1).
       environment division.
       input-output section.
       file-control.
           select f assign to "f.dat".
           select s assign to "s.tmp".
       data division.
       file section.
       fd  f.
       01  rf1 pic x.
       sd  s.
       01  rs1 pic x.
       procedure division.
       declaratives.
       d1 section.
           use after error procedure on f display 'x'.
       d1p.
           display 'd'.
       end declaratives.
       m section.
       m1.
           stop run.
