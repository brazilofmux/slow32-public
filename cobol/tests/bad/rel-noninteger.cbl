       identification division.
       program-id. rel-noninteger.
      * Relation conditions (X3.23-1985 6.3.1.1; 2023 8.8.4.2): a noninteger numeric beside text.
       data division.
       working-storage section.
       01 xs pic x(3).
       01 c4 pic 9(4) comp.
       01 d4 pic s9(4)v9.
       procedure division.
           if d4 = xs display "y" end-if.
           stop run.
