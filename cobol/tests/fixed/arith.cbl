       identification division.
       program-id. arith.
      * ADD, SUBTRACT, MULTIPLY, DIVIDE on the hot integer path and on the
      * numeric stack.  Uses COMP-3: default dialect.
       data division.
       working-storage section.
       01  a       pic s9(8) comp value 100.
       01  b       pic s9(8) comp value 23.
       01  c       pic s9(8) comp.
       01  d       pic s9(8) comp.
       01  small   pic 9(3) comp value 999.
       01  dd      pic 9(5) value 250.
       01  sdd     pic s9(5) value -7.
       01  pk      pic s9(7)v99 comp-3 value 1234.56.
       01  sc      pic s9(3)v99 comp value 1.25.
       01  r       pic s9(5)v9(3).
       01  n       pic s9(4) comp.
       procedure division.
           add b to a.
           display a.
           add 1 to a b.
           display a ' ' b.
           add a b to c giving d.
           display d.
           add 1 to small.
           display small.
           subtract 5 from a.
           display a.
           subtract a from b giving c.
           display c.
           subtract 1 2 3 from a b.
           display a ' ' b.
           add dd to a.
           display a.
           add sdd to a.
           display a.
           add a to dd.
           display dd.
           add 1.75 to pk.
           display pk.
           add pk to sc.
           display sc.
           add sc pk giving r.
           display r.
           subtract pk from r.
           display r.
           multiply 3 by a.
           display a.
           multiply a by 2 giving c.
           display c.
           multiply sc by pk giving r.
           display r.
           divide 7 into a.
           display a.
           divide a by 3 giving c.
           display c.
           divide 3 into a giving r.
           display r.
           divide pk by 4 giving sc.
           display sc.
           move -50 to n.
           add 1 to n.
           display n.
           subtract 1 from n giving c.
           display c.
           move 32767 to n.
           add 1 to n.
           display n.
           stop run.
