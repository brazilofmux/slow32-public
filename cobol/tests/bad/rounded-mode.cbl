       identification division.
       program-id. rmode.
       data division.
       working-storage section.
       01  a  pic s9(5)v99 value 1.
       procedure division.
           compute a rounded mode nearest-even = a / 3.
           stop run.
