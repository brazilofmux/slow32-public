       identification division.
       program-id. s02con.
      * The constant entry (2023 13.10) is not implemented: refused,
      * naming it, where it was "unexpected 'constant'".
       data division.
       working-storage section.
       01 k constant as 42.
       procedure division.
           stop run.
