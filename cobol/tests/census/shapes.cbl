       identification division.
       program-id. shapes.
       data division.
       working-storage section.
       01 rec-a.
          05 a-1 pic 9(4).
          05 a-2 pic x(4).
       01 rec-b redefines rec-a.
          05 b-1 pic x(2).
          05 b-2 pic x(6).
       01 rec-c.
          05 c-1 pic 9(4).
          05 c-2 pic x(4).
       01 rec-d redefines rec-c.
          05 d-1 pic x(2).
          05 d-2 pic x(6).
       01 g-shared global.
          05 gs-1 pic 9(4).
          05 gs-2 pic 9(4).
       01 g-quiet global.
          05 gq-1 pic 9(4).
       01 corr-a.
          05 k1 pic 9(2).
          05 k2 pic 9(2).
       01 corr-b.
          05 k1 pic 9(2).
          05 k2 pic 9(2).
       01 grp88.
          88 grp88-blank value spaces.
          05 g88-1 pic x(2).
       01 tbl.
          05 ent occurs 5 indexed by ex.
             10 e-key pic 9(2).
             10 e-val pic 9(4).
       01 ptr usage pointer.
       01 addr-of pic x(4).
       01 inner-tbl.
          05 outer-row occurs 3.
             10 o-a pic 9(2).
             10 o-in occurs 4.
                15 o-b pic 9(2).
                15 o-c pic x(2).
                15 o-d redefines o-c pic 9(2).
             10 o-e pic 9(2).
       01 str-src pic x(20).
       01 str-ptr pic 9(4) comp.
       01 uns-a pic x(5).
       01 uns-n pic 9(2).
       procedure division.
           move 1 to a-1
           move "ab" to b-1
           move 2 to c-1
           move "abcd" to c-2
           move 5 to gq-1
           move corresponding corr-a to corr-b
           if grp88-blank display "b" end-if
           move "zz" to g88-1
           search ent
              when e-key(ex) = 5 display e-val(ex)
           end-search
           set ptr to address of addr-of
           move 1 to o-a(1) o-b(1, 2) o-e(2)
           move "ab" to o-c(1, 1)
           move 1 to str-ptr
           string "abc" delimited by size into str-src with pointer str-ptr
           unstring str-src delimited by "b" into uns-a count in uns-n
           call "inner"
           stop run.
       identification division.
       program-id. inner.
       procedure division.
           add 1 to gs-1
           goback.
       end program inner.
       end program shapes.
