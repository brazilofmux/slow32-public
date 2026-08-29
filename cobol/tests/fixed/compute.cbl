       identification division.
       program-id. compute-test.
      * COMPUTE, ROUNDED, ON SIZE ERROR, REMAINDER, expressions in
      * conditions, and majesty's amount picture round-tripping through
      * COMP-3.  Default dialect (COMP-3).
       data division.
       working-storage section.
       01  a       pic s9(5)v99 value 100.50.
       01  b       pic s9(5)v99 value 3.
       01  c       pic s9(5)v99.
       01  d       pic s9(7)v9(4).
       01  n       pic s9(4) comp value 7.
       01  m       pic s9(4) comp.
       01  r       pic s9(4) comp.
       01  small   pic 9(2).
       01  amt     pic s9(9)v99 comp-3 value -1234567.89.
       01  amt2    pic s9(9)v99 comp-3.
       01  tot     pic s9(9)v99 comp-3 value 0.
       01  ed      pic ----,---,--9.99.
       01  flag    pic x.
       procedure division.
           compute c = a + b * 2.
           display c.
           compute c = (a + b) * 2.
           display c.
           compute c = a / b.
           display c.
           compute d = a / b.
           display d.
           compute c rounded = a / b.
           display c.
           compute c = a - b - 1.5.
           display c.
           compute c = -a.
           display c.
           compute c = 2 ** 10.
           display c.
           compute c = n * n - 1.
           display c.
           compute m = n / 2.
           display m.
           compute m rounded = n / 2.
           display m.
           compute c = 10 / 4 * 4.
           display c.
           compute c = 1 / 3 * 3.
           display c.
           move 0 to small.
           compute small = 99 + 1
               on size error display 'size error'
               not on size error display 'no size error'
           end-compute.
           display small.
           compute small = 99 + 1.
           display small.
           add 1 to n giving small
               on size error display 'x'
               not on size error display 'ok ' small
           end-add.
           move 0 to m.
           compute c = a / m
               on size error display 'div by zero'
           end-compute.
           display c.
           divide 7 into 100 giving m remainder r.
           display m ' ' r.
           divide 100 by 7 giving m remainder r.
           display m ' ' r.
           divide -100 by 7 giving m remainder r.
           display m ' ' r.
           divide 10 by 4 giving c rounded.
           display c.
           multiply 1.5 by a rounded.
           display a.
           add 0.005 to a rounded.
           display a.
           subtract 0.005 from a rounded.
           display a.
           if a + 1 > 152 display 'expr gt' end-if.
           if (a + b) * 2 = 313.50 display 'expr eq' end-if.
           if a > b + 100 display 'x' else display 'expr not gt' end-if.
           if n * 2 = 14 and (n + 1) = 8 display 'both' end-if.
           move amt to ed.
           display '[' ed ']'.
           move ed to amt2.
           display amt2.
           if amt = amt2 display 'round trip' end-if.
           compute tot = amt + amt2 + 0.01.
           display tot.
           compute tot rounded = tot / 3.
           display tot.
           compute ed = a * 1000.
           display '[' ed ']'.
           add a b giving ed.
           display '[' ed ']'.
           if amt < 0 move 'N' to flag else move 'P' to flag end-if.
           display flag.
           stop run.
