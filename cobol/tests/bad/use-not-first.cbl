       identification division.
       program-id. un.
      * USE immediately follows its section header (X3.23-1985 and
      * 2023 14.9.49.3 rule 1).
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
           display 'x'.
           use after error procedure on f.
       d1p.
           display 'd'.
       end declaratives.
       m section.
       m1.
           stop run.
