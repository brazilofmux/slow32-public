       identification division.
       program-id. stringtest.
      * STRING: DELIMITED BY SIZE / SPACE / literal, WITH POINTER, ON
      * OVERFLOW, and the case intrinsics.
       data division.
       working-storage section.
       01  out     pic x(20).
       01  small   pic x(6).
       01  ptr     pic 99.
       01  a       pic x(10) value 'Hello     '.
       01  b       pic x(10) value 'World'.
       01  c       pic x(5)  value 'ab,cd'.
       procedure division.
           move spaces to out.
           string a delimited by space ', ' delimited by size
                  b delimited by space '!' delimited by size
                  into out.
           display '[' out ']'.
           move spaces to out.
           move 1 to ptr.
           string 'ab' delimited by size into out with pointer ptr.
           string 'cd' delimited by size into out with pointer ptr.
           display '[' out '] ptr=' ptr.
           move all '*' to small.
           string 'abcdefgh' delimited by size into small
               on overflow display 'overflow [' small ']'
               not on overflow display 'fits'
           end-string.
           move spaces to out.
           string c delimited by ',' into out.
           display '[' out ']'.
           move spaces to out.
           string function upper-case(a) function lower-case('MiXeD')
                  delimited by size into out.
           display '[' out ']'.
           move function upper-case(b) to out.
           display '[' out ']'.
           display function lower-case('ABC') function upper-case(c).
           move 7 to ptr.
           move spaces to out.
           string 'xyz' delimited by size into out pointer ptr.
           display '[' out '] ptr=' ptr.
           stop run.
