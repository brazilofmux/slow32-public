       identification division.
       program-id. idxop.
      * An index-name is not data (X3.23-1985 OCCURS syntax rule 13;
      * 2023 13.18.38.3 rule 7: a subscript, PERFORM or SEARCH VARYING,
      * SET, a relation): not an operand of ADD or DISPLAY.
       data division.
       working-storage section.
       01 g.
          05 t pic x occurs 3 indexed by i.
       procedure division.
           add 1 to i.
           display i.
           stop run.
