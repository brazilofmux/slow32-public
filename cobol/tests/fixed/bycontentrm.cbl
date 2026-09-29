       identification division.
       program-id. bycontentrm.
      * CALL BY CONTENT of a reference-modified item (X3.23-1985 and
      * 2023 CALL rules put no restriction on it; cobol ISSUES-94, a
      * Stage A gap): the callee gets a copy of the part, literal and
      * computed positions, and its writes do not reach the caller.
       data division.
       working-storage section.
       01 a pic x(10) value "abcdefghij".
       01 k pic 99 value 3.
       01 l pic 99 value 4.
       procedure division.
           call "bcsub" using by content a(2:3).
           call "bcsub" using by content a(k:l).
           display "caller's a: " a.
           stop run.
       identification division.
       program-id. bcsub.
       data division.
       linkage section.
       01 p pic x(4).
       procedure division using p.
           display "callee got [" p(1:3) "]".
           move "ZZZZ" to p.
           exit program.
       end program bcsub.
       end program bycontentrm.
