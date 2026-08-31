       identification division.
       program-id. hotwrap.
       data division.
       working-storage section.
       01 a pic s9(9) comp value 999999999.
       01 b pic s9(9) comp value 999999999.
       01 c pic s9(9) comp value 999999999.
       01 d pic s9(9) comp.
       01 u pic 9(4) comp value 0.
       procedure division.
           add a b c giving d
           display d
           subtract 1 from u
           display u
           stop run.
