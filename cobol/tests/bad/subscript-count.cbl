       identification division.
       program-id. subcount.
       data division.
       working-storage section.
       01  tbl.
           05  e occurs 5 times pic 99.
       procedure division.
           display e.
           stop run.
