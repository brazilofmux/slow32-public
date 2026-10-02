       identification division.
       program-id. alone.
       environment division.
       input-output section.
       file-control.
           select f1 assign to "f1.dat" file status is fs1.
       data division.
       file section.
       fd f1.
       01 f1-rec pic x(20).
       working-storage section.
       77 n pic 9(4) comp.
       77 unused pic 9(4).
       01 fs1 pic xx.
       01 counters.
          05 c-a pic 9(4) comp value 0.
          05 c-b pic 9(5).
          05 c-c pic s9(7)v99 comp-3.
       01 inited.
          05 i-a pic 9(4).
          05 i-b pic x(10).
       01 moved.
          05 m-a pic 9(4).
          05 m-b pic x(3).
       01 base-area.
          05 b-text pic x(8).
          05 b-num redefines b-text pic 9(8).
          05 b-other pic x(4).
          05 b-only redefines b-other pic 9(4).
       01 tbl.
          05 row occurs 10 indexed by ix.
             10 r-key pic 9(3).
             10 r-val pic x(5).
             10 r-alt redefines r-val pic 9(5).
       01 tbl2.
          05 row2 occurs 10.
             10 q-key pic 9(3).
             10 q-val pic x(5).
       01 rn.
          05 rn-a pic x(2).
          05 rn-b pic x(2).
          05 rn-c pic x(2).
       66 rn-ab renames rn-a thru rn-b.
       01 flag pic x.
          88 flag-on value "Y".
       01 arg1 pic 9(4) comp.
       01 arg2 pic 9(4) comp.
       01 txt pic x(30).
       01 clsd pic 9(3).
       01 odo-n pic 9(2).
       01 odo-t.
          05 odo-e pic x occurs 1 to 20 depending on odo-n.
       local-storage section.
       01 ls-n pic 9(4) comp.
       procedure division.
           add 1 to n c-a c-b
           move 1.5 to c-c
           initialize inited
           move 5 to i-a
           move "x" to i-b
           move spaces to moved
           move 7 to m-a
           move "12345678" to b-text
           display b-num
           move 12 to b-only
           perform varying n from 1 by 1 until n > 10
              move n to r-key(n)
              move "ab" to r-val(n)
              move n to q-key(n)
           end-perform
           set ix to 3
           display r-alt(ix) q-val(2)
           move "ab" to rn-ab
           display rn-a rn-c
           if flag-on display "on" end-if
           call "sub" using arg1 by content arg2
           move txt(2:3) to rn-c
           if clsd is numeric display "n" end-if
           move 3 to odo-n
           display odo-e(2)
           move 1 to ls-n
           open input f1
           close f1
           stop run.
