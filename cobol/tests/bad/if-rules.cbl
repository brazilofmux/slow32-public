       identification division.
       program-id. ifrul.
      * IF (X3.23-1985 IF syntax rules 1 and 3): a statement or NEXT
      * SENTENCE after the condition and after ELSE; NEXT SENTENCE only
      * without END-IF.
       data division.
       working-storage section.
       01 a pic 9 value 1.
       procedure division.
           if a = 1 end-if.
           if a = 1 continue else end-if.
           if a = 1 next sentence end-if.
           stop run.
