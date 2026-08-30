       identification division.
       program-id. odomove.
       data division.
       working-storage section.
       01  n   pic 99 value 2.
       01  tbl.
           05  e occurs 1 to 5 times depending on n pic x(3).
       01  x   pic x(15).
       procedure division.
           move tbl to x.
           stop run.
