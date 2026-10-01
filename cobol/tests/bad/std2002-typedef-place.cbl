       identification division.
       program-id. s02tdp.
      * TYPEDEF comes immediately after the data-name (2023 13.16.3
      * rule 4).  Accepted before the DATA DIVISION sweep (2026-09-30).
       data division.
       working-storage section.
       01 t pic x(4) typedef.
       procedure division.
           stop run.
