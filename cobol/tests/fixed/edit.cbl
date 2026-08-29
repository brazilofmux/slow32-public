       identification division.
       program-id. edit.
      * Editing and de-editing: every picture majesty prints, plus the
      * ones the 1985 tables make interesting, each with a negative, a
      * small value and zero.  ----,---,--9.99 is gl030's picture and the
      * one cobc370 got wrong for small negatives.
       data division.
       working-storage section.
       01  v       pic s9(9)v99.
       01  i       pic s9(9).
       01  e1      pic ----,---,--9.99.
       01  e2      pic ---,---,--9.99.
       01  e3      pic +99999.
       01  e4      pic +9(8).
       01  e5      pic zz9.
       01  e6      pic 99.99.
       01  e7      pic z9.99.
       01  e8      pic z(8)9.
       01  e9      pic $zz9.99-.
       01  e10     pic -9(9).
       01  e11     pic 9999.99-.
       01  e12     pic +z(8)9.99.
       01  e13     pic $$$,$$9.99.
       01  e14     pic **,***.99.
       01  e15     pic 9(3)cr.
       01  e16     pic 9(3)db.
       01  e17     pic zzz.zz.
       01  e18     pic 99/99/99.
       01  e19     pic 9(3)b9(3).
       01  e20     pic zzz9 blank when zero.
       01  e21     pic ++++9.
       01  e22     pic 9(3) value 7.
       01  a1      pic x(4)/x(2).
       01  a2      pic xxbxx.
       01  a3      pic xxx0x.
       01  back    pic s9(9)v99.
       01  cmp     pic s9(5)v99 comp.
       procedure division.
           perform evaluate-value varying i from 1 by 1 until i > 6.
           display 'fixed:'.
           move 7 to e22 e5 e8.
           display '[' e22 '][' e5 '][' e8 ']'.
           move 0 to e20.
           display '[' e20 ']'.
           move 5 to e20.
           display '[' e20 ']'.
           move 12 to e21.
           display '[' e21 ']'.
           move -12 to e21.
           display '[' e21 ']'.
           move 'abcdef' to a1.
           display '[' a1 ']'.
           move 'abcd' to a2.
           display '[' a2 ']'.
           move 'ab' to a3.
           display '[' a3 ']'.
           move 1234 to a1.
           display '[' a1 ']'.
           display 'de-edit:'.
           move -1234567.89 to e1.
           move e1 to back.
           display back.
           move e1 to cmp.
           display cmp.
           move -5 to e9.
           move e9 to back.
           display back.
           move 42 to e15.
           move e15 to back.
           display back.
           move -42 to e15.
           move e15 to back.
           display back.
           move 123456 to e13.
           move e13 to back.
           display back.
           stop run.
       evaluate-value.
           if i = 1 move -1234567.89 to v.
           if i = 2 move -5 to v.
           if i = 3 move -0.05 to v.
           if i = 4 move 0 to v.
           if i = 5 move 987654321.12 to v.
           if i = 6 move 12.34 to v.
           move v to e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13 e14
                     e15 e16 e17 e18 e19 e20.
           display 'v=' v.
           display '[' e1 '][' e2 '][' e3 '][' e4 ']'.
           display '[' e5 '][' e6 '][' e7 '][' e8 ']'.
           display '[' e9 '][' e10 '][' e11 '][' e12 ']'.
           display '[' e13 '][' e14 '][' e15 '][' e16 ']'.
           display '[' e17 '][' e18 '][' e19 '][' e20 ']'.
