       identification division.
       program-id. refmod-arith.
       data division.
       working-storage section.
       01 x pic 9(5) value 12345.
       01 y pic 9(5).
       procedure division.
           compute y = x(1:3)
           stop run.
