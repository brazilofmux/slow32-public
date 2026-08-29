       identification division.
       program-id. moves.
      * The conversion matrix, unedited cells.  Uses COMP-3, so the
      * expected output is GnuCOBOL default dialect.
       data division.
       working-storage section.
       01  x5      pic x(5).
       01  x8      pic x(8).
       01  xj      pic x(8) justified right.
       01  d5      pic 9(5).
       01  sd5     pic s9(5).
       01  d32     pic 9(3)v99.
       01  sd32    pic s9(3)v99.
       01  c4      pic 9(4) comp.
       01  sc9     pic s9(9) comp.
       01  sc18    pic s9(18) comp.
       01  p52     pic s9(5)v99 comp-3.
       01  p3      pic 9(3) comp-3.
       01  grp.
           05  g1  pic x(3).
           05  g2  pic 9(3).
       procedure division.
           move 'abcdefghij' to x5.
           display '[' x5 ']'.
           move 'ab' to x8.
           display '[' x8 ']'.
           move 'ab' to xj.
           display '[' xj ']'.
           move x5 to xj.
           display '[' xj ']'.
           move 123 to d5.
           display d5.
           move -123 to sd5.
           display sd5.
           move 123456 to d5.
           display d5.
           move 12.345 to d32.
           display d32.
           move -1.5 to sd32.
           display sd32.
           move sd32 to d32.
           display d32.
           move d32 to sd5.
           display sd5.
           move 9999 to c4.
           display c4.
           move 12345 to c4.
           display c4.
           move -123456789 to sc9.
           display sc9.
           move sc9 to sc18.
           display sc18.
           move 123456789012345678 to sc18.
           display sc18.
           move sc18 to c4.
           display c4.
           move -12345.67 to p52.
           display p52.
           move p52 to sd32.
           display sd32.
           move p52 to sc9.
           display sc9.
           move sc9 to p3.
           display p3.
           move sc9 to x8.
           display '[' x8 ']'.
           move d5 to x8.
           display '[' x8 ']'.
           move 'zzz' to g1.
           move 5 to g2.
           display grp.
           move grp to x8.
           display '[' x8 ']'.
           move spaces to x5.
           display '[' x5 ']'.
           move zeros to d5.
           display d5.
           move zero to sc9.
           display sc9.
           move all 'ab' to x5.
           display x5.
           move high-values to x5.
           if x5 = high-values display 'high' end-if.
           move 7 to sc9 d5 p3.
           display sc9 ' ' d5 ' ' p3.
           stop run.
