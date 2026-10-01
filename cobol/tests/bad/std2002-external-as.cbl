       identification division.
       program-id. s02exas.
      * EXTERNAL AS literal, an externalized name (2023 13.18.22), is
      * not implemented: refused, naming it.
       data division.
       working-storage section.
       01 a pic x(4) external as "shared-a".
       procedure division.
           stop run.
