       identification division.
       program-id. bsort.
      *> SORT benchmark: generates N records with an LCG, sorts them on
      *> four keys of four usages (alnum asc, COMP-3 desc, signed DISPLAY
      *> asc, COMP asc), prints a checksum of the output order.  Self-
      *> contained: no input file.  N from the command line, default 20000.
       environment division.
       input-output section.
       file-control.
           select work-file assign to 'bench/tmp/bsort-work.tmp'.
       data division.
       file section.
       sd  work-file.
       01  wr.
           05  wr-dept      pic x(4).
           05  wr-amount    pic s9(7)v99 comp-3.
           05  wr-bal       pic s9(9).
           05  wr-seq       pic s9(9) comp.
           05  wr-pad       pic x(40).
       working-storage section.
       01  n            pic 9(9) comp value 20000.
       01  i            pic 9(9) comp.
       01  seed         pic 9(9) comp value 12345.
       01  r            pic 9(9) comp.
       01  q            pic 9(9) comp.
       01  ck           pic 9(9) comp value 0.
       01  prev-seq     pic s9(9) comp.
       01  arg          pic x(12).
       01  ck-ed        pic z(9)9.
       01  n-ed         pic z(9)9.
       01  depts        pic x(16) value 'ACCTSALEMFGRHRSQ'.
       01  d-ix         pic 9(4) comp.
       01  eof          pic x value 'n'.
       01  t1           pic s9(18) comp.
       procedure division.
       main.
           accept arg from command-line
           if arg not = spaces
               compute n = function numval(arg)
           end-if
           sort work-file
               on ascending key wr-dept
               on descending key wr-amount
               on ascending key wr-bal
               on ascending key wr-seq
               with duplicates in order
               input procedure is gen
               output procedure is sum
           move ck to ck-ed
           move n to n-ed
           display 'bsort n=' n-ed ' checksum=' ck-ed
           stop run.
       gen.
           perform varying i from 1 by 1 until i > n
               compute t1 = seed * 1103515245 + 12345
               compute seed = function mod(t1, 2147483648)
               compute r = seed / 65536
               compute q = function mod(r, 4)
               compute d-ix = q * 4 + 1
               move depts(d-ix:4) to wr-dept
               compute t1 = function mod(r, 200000) - 100000
               compute wr-amount = t1 / 100
               compute wr-bal = function mod(r * 7, 1000) - 500
               move i to wr-seq
               move all 'x' to wr-pad
               release wr
           end-perform.
       sum.
           move -1 to prev-seq
           move 'n' to eof
           perform until eof = 'y'
               return work-file
                   at end move 'y' to eof
                   not at end
                       compute t1 = wr-bal + 500
                       compute t1 = function mod(t1, 1000)
                       compute t1 = ck * 31 + wr-seq + t1
                       compute ck = function mod(t1, 1000000007)
               end-return
           end-perform.
