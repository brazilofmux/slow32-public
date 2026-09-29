       identification division.
       program-id. clsrul.
      * JUSTIFIED, SYNCHRONIZED and SIGN (X3.23-1985 5.6.3 rules 1 and
      * 3, 5.13.3 rule 1, 5.12.3 rule 1): JUSTIFIED on an elementary,
      * unedited item; SYNCHRONIZED on an elementary item (COBOL 85; 2002
      * allows a group); a group's SIGN over a signed numeric item.
       data division.
       working-storage section.
       01 g1 justified.
          05 a1 pic x.
       01 e1 pic x/x justified.
       01 g2 sync.
          05 a2 pic x.
       01 g3 sign leading.
          05 a3 pic x.
       procedure division.
           stop run.
