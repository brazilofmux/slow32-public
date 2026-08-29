       identification division.
       program-id. ambig.
       data division.
       working-storage section.
       01  rec-a.
           05  id  pic x(4).
       01  rec-b.
           05  id  pic x(4).
       procedure division.
           display id.
           stop run.
