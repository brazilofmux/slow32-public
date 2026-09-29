       identification division.
       program-id. unsrm.
      * In COBOL 85 the UNSTRING sending item shall not be
      * reference-modified (X3.23-1985 UNSTRING syntax rule 7); 2023
      * dropped the rule, so -std=2002 accepts it.
       data division.
       working-storage section.
       01 a pic x(10) value "ab,cd,ef".
       01 x pic x(4).
       01 y pic x(4).
       procedure division.
           unstring a(4:5) delimited by "," into x y.
           stop run.
