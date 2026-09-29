       identification division.
       program-id. expg.
      * EXIT PROGRAM is not in a declarative procedure whose USE has the
      * GLOBAL phrase (X3.23-1985 EXIT PROGRAM syntax rule 2; 2023
      * 14.9.14.3 rule 2).
       environment division.
       input-output section.
       file-control.
           select f assign to "x.dat".
       data division.
       file section.
       fd  f.
       01  r pic x.
       procedure division.
       declaratives.
       d1 section.
           use global after error procedure on f.
       d1p.
           exit program.
       end declaratives.
       m section.
       m1.
           stop run.
