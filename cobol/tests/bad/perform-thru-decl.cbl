       identification division.
       program-id. ptd.
      * A PERFORM range that names a declarative procedure stays within
      * one declarative section (X3.23-1985 PERFORM syntax rule 11;
      * 2023 14.9.28.3 rule 11).
       environment division.
       input-output section.
       file-control.
           select f assign to "x.dat".
           select g assign to "y.dat".
       data division.
       file section.
       fd  f.
       01  recf pic x.
       fd  g.
       01  recg pic x.
       procedure division.
       declaratives.
       d1 section.
           use after error procedure on f.
       d1a.
           display "d1".
       d2 section.
           use after error procedure on g.
       d2a.
           display "d2".
       end declaratives.
       m section.
       m1.
           perform d1a thru d2a.
           stop run.
