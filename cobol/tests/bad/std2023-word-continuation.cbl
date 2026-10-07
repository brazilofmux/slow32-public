       identification division.
       program-id. p-word-continuation.
      * COBOL 2023 removed the continuation of words across fixed-form lines
      * (Annex E.2 item 1; BP-R1).
       data division.
       working-storage section.
       01 long-
      -    name pic x(3) value "abc".
       procedure division.
           display long-name
           stop run.
