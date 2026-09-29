       identification division.
       program-id. rwcode.
      * The Report Writer CODE clause (X3.23-1985 XIII 3.6; 2023
      * 13.18.12; cobol ISSUES-94, a Stage A gap): two reports share one
      * print file, and each record begins with its report's code,
      * outside the lines' columns.  The file is read back.  No oracle:
      * GnuCOBOL 4.0-early-dev writes an empty print file for a report
      * with CODE (docs/oracles.md).
       environment division.
       input-output section.
       file-control.
           select outf assign to "tmp/rwcode.prn"
               organization is line sequential.
           select backf assign to "tmp/rwcode.prn"
               organization is line sequential.
       data division.
       file section.
       fd  outf report is ra rb.
       fd  backf.
       01  bl pic x(30).
       working-storage section.
       01  n pic 9 value 0.
       01  eof pic x value "n".
       report section.
       rd  ra code "A1".
       01  da type detail line plus 1.
           05 column 1 pic x(8) value "report a".
           05 column 10 pic 9 source n.
       rd  rb code "B2".
       01  db type detail line plus 1.
           05 column 3 pic x(8) value "report b".
           05 column 12 pic 9 source n.
       procedure division.
           open output outf.
           initiate ra rb.
           move 1 to n.
           generate da.
           generate db.
           move 2 to n.
           generate da.
           terminate ra rb.
           close outf.
           open input backf.
           perform until eof = "y"
               read backf at end move "y" to eof
                   not at end display "[" bl "]"
               end-read
           end-perform.
           close backf.
           stop run.
