       identification division.
       program-id. rmode.
      * ROUNDED MODE names one of eight modes (2023 14.7.4.2); taken
      * as BP-E29 (2002/rmode), an unknown one is refused.
       data division.
       working-storage section.
       01  a  pic s9(5)v99 value 1.
       procedure division.
           compute a rounded mode half-up = a / 3.
           stop run.
