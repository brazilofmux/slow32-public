       identification division.
       program-id. glbrul.
      * GLOBAL (X3.23-1985 X-21, data description entry syntax rule 5;
      * X-24, GLOBAL syntax rules 1 and 2): a level 01 entry in the file
      * or working-storage section, with a data-name, its name not that
      * of another GLOBAL item.  Accepted before the DATA DIVISION sweep
      * (2026-09-30).
       data division.
       working-storage section.
       01 g.
          05 g1 pic x global.
       77 g2 pic x global.
       01 filler pic x global.
       01 g3 pic x global.
       01 g3 pic x global.
       linkage section.
       01 g4 pic x global.
       procedure division.
           stop run.
