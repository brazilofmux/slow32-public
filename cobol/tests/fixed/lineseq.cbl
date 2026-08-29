       identification division.
       program-id. lineseq.
      * Line sequential in and out, ASSIGN to a data-name built with
      * STRING and FUNCTION LOWER-CASE, READ INTO, WRITE FROM, FILE
      * STATUS, OPTIONAL, a fixed sequential round trip.  SHARING is a
      * GnuCOBOL extension -std=cobol85 rejects: default dialect.
       environment division.
       input-output section.
       file-control.
           select desc-file assign to 'data/descriptions.txt'
               organization is line sequential
               access is sequential
               sharing with all other.
           select copy-file assign to ws-copy-name
               organization is line sequential
               file status is copy-status.
           select optional maybe-file assign to 'data/absent.txt'
               organization is line sequential
               file status is maybe-status.
           select missing-file assign to 'data/absent.txt'
               organization is line sequential
               file status is missing-status.
           select fixed-file assign to 'tmp/fixed.dat'
               organization is sequential.
           select companies-file assign to 'data/companies.txt'
               organization is line sequential.
       data division.
       file section.
       fd  desc-file
           block contains 2000 records.
       01  desc-record.
           05  d-id    pic 9(10).
           05  d-text  pic x(40).
       fd  copy-file.
       01  copy-record.
           05  c-id    pic 9(10).
           05  c-text  pic x(40).
       fd  maybe-file.
       01  maybe-record pic x(20).
       fd  missing-file.
       01  missing-record pic x(20).
       fd  fixed-file
           record contains 12 characters.
       01  fixed-record.
           05  f-key   pic 9(4).
           05  f-amt   pic s9(5)v99 comp-3.
           05  f-tag   pic x(4).
       fd  companies-file.
       01  company-record.
           05  com-id      pic 99.
           05  com-code    pic x(3).
           05  com-re-id   pic 9(5).
           05  com-name    pic x(30).
       working-storage section.
       01  ws-copy-name    pic x(40).
       01  copy-status     pic xx.
       01  maybe-status    pic xx.
       01  missing-status  pic xx.
       01  eof             pic x value 'N'.
       01  n-read          pic 9(4) comp value 0.
       01  n-empty         pic 9(4) comp value 0.
       01  ws-desc.
           05  w-id    pic 9(10).
           05  w-text  pic x(40).
       01  i               pic 9(4) comp.
       procedure division.
       main-para.
           string 'tmp/copy-' function lower-case('DeSc') '.txt'
               delimited by size into ws-copy-name.
           display 'copy name=[' ws-copy-name ']'.
           open input desc-file.
           open output copy-file.
           display 'open copy: ' copy-status.
           perform until eof = 'Y'
               read desc-file into ws-desc
                   at end move 'Y' to eof
                   not at end
                       add 1 to n-read
                       if w-text = spaces add 1 to n-empty end-if
                       write copy-record from ws-desc
               end-read
           end-perform.
           close desc-file copy-file.
           display 'read ' n-read ' records, ' n-empty ' empty; close: '
                   copy-status.
           display 'first id=' w-id.
      * read the copy back and show a few
           move 'N' to eof.
           move 0 to n-read.
           open input copy-file.
           perform until eof = 'Y'
               read copy-file
                   at end move 'Y' to eof
                   not at end
                       add 1 to n-read
                       if n-read < 4 or n-read = 5
                           display c-id ' [' c-text ']'
                       end-if
               end-read
           end-perform.
           close copy-file.
           display 'copy has ' n-read.
      * OPTIONAL absent: open succeeds, the first read is at end
           open input maybe-file.
           display 'optional open: ' maybe-status.
           read maybe-file
               at end display 'optional at end: ' maybe-status
               not at end display 'x'
           end-read.
           close maybe-file.
      * not optional: status 35
           open input missing-file.
           display 'missing open: ' missing-status.
      * fixed sequential: 12-byte records, no delimiter, packed inside
           open output fixed-file.
           perform varying i from 1 by 1 until i > 3
               move i to f-key
               compute f-amt = i * -123.45
               move 'abcd' to f-tag
               write fixed-record
           end-perform.
           close fixed-file.
           open input fixed-file.
           move 'N' to eof.
           perform until eof = 'Y'
               read fixed-file
                   at end move 'Y' to eof
                   not at end display f-key ' ' f-amt ' ' f-tag
               end-read
           end-perform.
           close fixed-file.
      * companies, the gl022 shape, and the per-company filename
           open input companies-file.
           move 'N' to eof.
           perform until eof = 'Y'
               read companies-file
                   at end move 'Y' to eof
                   not at end
                       string 'reports_cobol/chartofaccounts1-'
                              function lower-case(com-code) '.prn'
                              delimited by size into ws-copy-name
                       display com-id ' ' ws-copy-name
               end-read
           end-perform.
           close companies-file.
           stop run.
