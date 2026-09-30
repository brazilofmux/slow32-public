       identification division.
       program-id. norewind.
      * OPEN and CLOSE ... [WITH] NO REWIND (X3.23-1985 sequential OPEN
      * and CLOSE formats): WITH is optional.  OPEN OUTPUT f NO REWIND
      * was refused ("'no' is not a file"); found walking Micro Focus's
      * RM/COBOL pages (docs/dialect.md).
       environment division.
       input-output section.
       file-control.
           select f assign to "nr.dat" organization sequential.
       data division.
       file section.
       fd f.
       01 fr pic x(5).
       procedure division.
           open output f no rewind.
           write fr from "abcde".
           close f no rewind.
           open input f with no rewind.
           read f.
           display fr.
           close f.
           stop run.
