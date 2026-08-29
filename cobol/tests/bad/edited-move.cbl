       identification division.
       program-id. edmove.
       data division.
       working-storage section.
       01  amt  pic s9(5)v99 comp-3 value -12.34.
       01  ed   pic ----,---,--9.99.
       procedure division.
           move amt to ed.
           display ed.
           stop run.
