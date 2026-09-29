       identification division.
       program-id. ur.
      * A declarative procedure refers to no nondeclarative one
      * (X3.23-1985 USE rules; 2023 14.9.49.3 rule 3).
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
           use after error procedure on f.
       d1p.
           go to m1.
       end declaratives.
       m section.
       m1.
           stop run.
