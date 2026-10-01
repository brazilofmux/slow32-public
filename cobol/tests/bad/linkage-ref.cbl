       identification division.
       program-id. lkref.
      * An item of the LINKAGE SECTION is referenced only as, or under,
      * a USING operand, or a REDEFINES of one (X3.23-1985 X-25 and
      * X-26, procedure division header rule 4).  Accepted before the
      * DATA DIVISION sweep (2026-09-30).
       data division.
       linkage section.
       01 a pic x.
       01 r redefines a pic x.
       01 b pic x.
       procedure division using a.
           move "x" to a r.
           move "y" to b.
           goback.
