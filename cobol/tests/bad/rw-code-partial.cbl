       identification division.
       program-id. rwcp.
      * If the CODE clause is specified for any report in a file, it
      * must be for each (X3.23-1985 XIII 3.6.3 rule 2; 2023 13.18.12.3
      * rule 3).
       environment division.
       input-output section.
       file-control.
           select outf assign to "tmp/rwcp.prn"
               organization is line sequential.
       data division.
       file section.
       fd  outf reports are ra rb.
       report section.
       rd  ra code "A1".
       01  da type detail line plus 1.
           05 column 1 pic x(4) value "a".
       rd  rb.
       01  db type detail line plus 1.
           05 column 1 pic x(4) value "b".
       procedure division.
           open output outf.
           initiate ra rb.
           generate da.
           terminate ra rb.
           close outf.
           stop run.
