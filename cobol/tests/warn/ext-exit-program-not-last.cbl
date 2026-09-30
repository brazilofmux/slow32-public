       identification division.
       program-id. expnl.
      * In COBOL 85 an EXIT PROGRAM in a sequence of imperative
      * statements is the last of them (X3.23-1985 EXIT PROGRAM syntax
      * rule 1; 2023 has no such rule).  Taken as BP-E21: the NIST SQL
      * suite's dml116s ends EXIT PROGRAM, STOP RUN.
       procedure division.
       m1.
           exit program display "after".
           stop run.
