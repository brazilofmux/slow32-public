       identification division.
       program-id. extrul.
      * EXTERNAL (X3.23-1985 X-21, data description entry syntax rules
      * 2, 3 and 5; X-23, EXTERNAL syntax rules 2 and 3; 2023 13.16.3
      * rules 5 and 7, 13.18.22.3 rules 1 and 2): a level 01 entry in
      * working storage, with a data-name, no REDEFINES, described once,
      * and under 85 no VALUE but on its 88s.  Accepted before the DATA
      * DIVISION sweep (2026-09-30).
       data division.
       working-storage section.
       01 g.
          05 e1 pic x(4) external.
       01 filler pic x(4) external.
       01 b pic x(4).
       01 e2 redefines b pic x(4) external.
       01 e3 pic x(4) external.
       01 e3 pic x(4) external.
       01 e4 external.
          05 v pic x value "a".
          05 w pic x.
             88 w-ok value "a".
       linkage section.
       01 e5 pic x(4) external.
       procedure division.
           stop run.
