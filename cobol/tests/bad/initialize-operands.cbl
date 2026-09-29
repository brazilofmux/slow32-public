       identification division.
       program-id. initop.
      * INITIALIZE's syntax rules (X3.23-1985 INITIALIZE syntax rules 3
      * and 6): a category once in REPLACING, no RENAMES item; an
      * index-name is not a data item at all; and the COBOL 2002 phrases
      * are not COBOL 85.
       data division.
       working-storage section.
       01 g.
          05 a pic x(3).
          05 n pic 9(3).
       66 rn renames a thru n.
       01 h.
          05 hd pic x occurs 3 indexed by hi.
       procedure division.
           initialize g replacing numeric by 1 numeric by 2.
           initialize rn.
           initialize hi.
           initialize g with filler.
           stop run.
