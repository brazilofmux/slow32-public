       identification division.
       program-id. strrm.
      * The STRING receiver shall not be reference-modified (X3.23-1985
      * STRING syntax rule 3; 2023 14.9.43.3 rule 4).
       data division.
       working-storage section.
       01 d pic x(10).
       procedure division.
           string "ab" delimited by size into d(3:4).
           stop run.
