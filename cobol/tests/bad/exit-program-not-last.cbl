       identification division.
       program-id. expnl.
      * In COBOL 85 an EXIT PROGRAM in a sequence of imperative
      * statements is the last of them (X3.23-1985 EXIT PROGRAM syntax
      * rule 1; 2023 has no such rule).
       procedure division.
       m1.
           exit program display "after".
           stop run.
