       identification division.
       program-id. redefl.
      * A redefinition is no larger than the item it redefines unless
      * that is a level 01 entry (X3.23-1985 REDEFINES syntax rule 6;
      * 2023 13.18.44.3 rule 8).
       data division.
       working-storage section.
       01 g.
          05 i pic x(4).
          05 j redefines i pic x(6).
       procedure division.
           stop run.
