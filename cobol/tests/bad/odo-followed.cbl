       identification division.
       program-id. odofol.
      * An OCCURS DEPENDING ON table may be followed in its record only
      * by entries subordinate to it (X3.23-1985 OCCURS format 2 syntax
      * rule 10; 2023 13.18.38.3 rule 22).  Refused at the declaration.
       data division.
       working-storage section.
       01 n pic 9 value 2.
       01 g.
          05 t pic x occurs 1 to 5 depending on n.
          05 after-t pic x.
       procedure division.
           display g.
           stop run.
