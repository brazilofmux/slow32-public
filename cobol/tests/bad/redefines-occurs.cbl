       identification division.
       program-id. redefo.
      * The redefined item has no OCCURS clause (X3.23-1985 REDEFINES
      * syntax rule 5; 2023 13.18.44.3 rule 5).
       data division.
       working-storage section.
       01 g.
          05 f pic x occurs 4.
          05 h redefines f pic x(4).
       procedure division.
           stop run.
