       identification division.
       program-id. nonint.
       data division.
       working-storage section.
       01  amt  pic s9(5)v99 value 1.5.
       01  x    pic x(8).
       procedure division.
           move amt to x.
           stop run.
