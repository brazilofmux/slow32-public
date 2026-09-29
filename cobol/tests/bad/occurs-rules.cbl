       identification division.
       program-id. occrul.
      * The OCCURS clause's rules (X3.23-1985 OCCURS syntax rules 1b,
      * 3, 5, 11 and 12; 2023 13.18.38.3 rules 1b, 3, 4, 6 and 16): a
      * key is the table's entry or below it, has no OCCURS of its own
      * and is not inside another table; no variable table below a
      * table; the maximum above the minimum.
       data division.
       working-storage section.
       01 k pic 9 value 1.
       01 g1.
          05 w pic x.
          05 t1 occurs 3 ascending key w.
             10 u1 pic x.
       01 g2.
          05 t2 occurs 3 ascending key v2.
             10 v2 pic x occurs 2.
       01 g3.
          05 t3 occurs 3 ascending key v3.
             10 h3 occurs 2.
                15 v3 pic x.
       01 g4.
          05 t4 occurs 3.
             10 u4 pic x occurs 1 to 3 depending on k.
       01 g5.
          05 t5 pic x occurs 3 to 3 depending on k.
       procedure division.
           stop run.
