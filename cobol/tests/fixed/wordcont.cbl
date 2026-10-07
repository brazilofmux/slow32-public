       identification division.
       program-id. wordcont.
      * A COBOL word continued across fixed-form lines (X3.23-1985 and 2002:
      * a hyphen in column 7 joins the text), the language through 2014, taken
      * under -std=85/2002/2014 as BP-R1; COBOL 2023 removed it (Annex E.2 item
      * 1; bad/std2023-word-continuation).  A literal continued is fine still
      * (free/..., the CCVS programs).
      * GnuCOBOL agrees.  docs/conformance/edition-2023.md
       data division.
       working-storage section.
       01 long-
      -    name pic x(3) value "abc".
       01 lit pic x(10) value "abcdef".
       procedure division.
           display long-name " " lit
           stop run.
