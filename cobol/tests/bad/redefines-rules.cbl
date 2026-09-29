       identification division.
       program-id. redefr.
      * Where a REDEFINES entry stands (X3.23-1985 REDEFINES syntax rules
      * 2 and 8; 2023 13.18.44.3 rules 2, 4, 7 and 10): the level of the
      * item it redefines, right after that item (no other storage
      * between), naming the original rather than another redefinition.
       data division.
       working-storage section.
       01 g.
          05 a pic x(4).
          05 z pic x.
          05 b redefines a pic x(4).
          05 c pic x(4).
          05 d redefines c pic x(4).
          05 e redefines d pic x(4).
          05 m.
             10 m1 pic x(4).
             10 n redefines m pic x(4).
       procedure division.
           stop run.
