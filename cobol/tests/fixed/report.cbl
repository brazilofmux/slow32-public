       identification division.
       program-id. report.
      * Report Writer, the cheap half: a page heading with LINE +1 / +2, a
      * one-line detail and a two-line detail, LINE PLUS 2, edited amounts
      * with a NEGATIVE, a total line, page breaks forced by a small PAGE
      * LIMIT, two INITIATE/TERMINATE cycles, then the print file read
      * back so the layout -- blank lines and page padding included -- is
      * in the output.  COMP-3 amounts: default dialect.
       environment division.
       input-output section.
       file-control.
           select print-file assign to ws-name
               organization is line sequential.
           select back-file assign to ws-name
               organization is line sequential.
       data division.
       file section.
       fd  print-file
           report is sales.
       fd  back-file.
       01  back-line pic x(90).
       working-storage section.
       01  ws-name     pic x(30).
       01  company     pic x(20).
       01  i           pic 99.
       01  item        pic x(12).
       01  amount      pic s9(7)v99 comp-3.
       01  total       pic s9(9)v99 comp-3.
       01  eof         pic x.
       01  n           pic 999.
       report section.
       rd  sales
           page limit is 12 lines
           heading 1
           first detail 5
           last detail 11.
       01  page-top type page heading.
           02  line +1.
               05  column 1  pic x(20) source company.
               05  column 30 pic x(11) value 'S A L E S'.
           02  line +1.
               05  column 30 pic x(8) value 'BY ITEM'.
           02  line +2.
               05  column 1 pic x(4) value 'ITEM'.
               05  column 20 pic x(6) value 'AMOUNT'.
       01  sale-line type detail.
           02  line plus 1.
               05  column 1  pic x(12) source item.
               05  column 18 pic ----,---,--9.99 source amount.
       01  note-line type detail.
           02  line plus 2.
               05  column 3  pic x(5) value 'note:'.
               05  column 9  pic 99 source i.
           02  line plus 1.
               05  column 3  pic x(9) value 'continued'.
       01  total-line type detail.
           02  line plus 2.
               05  column 1  pic x(6) value 'TOTAL:'.
               05  column 18 pic ----,---,--9.99 source total.
       procedure division.
           move 'tmp/report-a.prn' to ws-name.
           move 'Acme Widgets' to company.
           perform run-report.
           move 'tmp/report-b.prn' to ws-name.
           move 'Beta Ltd' to company.
           perform run-report.
           move 'tmp/report-a.prn' to ws-name.
           perform show-file.
           move 'tmp/report-b.prn' to ws-name.
           perform show-file.
           stop run.
       run-report.
           open output print-file.
           initiate sales.
           move 0 to total.
           perform varying i from 1 by 1 until i > 9
               move 'widget' to item
               compute amount = i * 1234.56 - 5000
               add amount to total
               generate sale-line
               if i = 4 generate note-line end-if
           end-perform.
           generate total-line.
           terminate sales.
           close print-file.
       show-file.
           display '=== ' ws-name.
           open input back-file.
           move 'N' to eof.
           move 0 to n.
           perform until eof = 'Y'
               read back-file
                   at end move 'Y' to eof
                   not at end
                       add 1 to n
                       display n ' [' back-line ']'
               end-read
           end-perform.
           close back-file.
