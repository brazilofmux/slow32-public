       identification division.
       program-id. values.
      * VALUE clauses across the usages majesty writes, each shown by
      * DISPLAY.  Expected output is GnuCOBOL default dialect: COMP-3,
      * COMP-5 and signed-int are implementor usages -std=cobol85 rejects.
       data division.
       working-storage section.
       77  a   pic s9(3) value -5.
       77  b   pic 9(3)v99 value 1.5.
       77  c   pic s9(5) comp value -42.
       77  d   pic s9(4)v99 comp-3 value -12.34.
       77  e   pic 9(4) comp-5 value 7.
       77  f   pic x(5) value 'ab'.
       77  g   pic s9(3) value 5.
       77  h   pic 99v9 comp value 1.5.
       77  i   signed-int value -9.
       77  j   pic 9(3).
       77  k   pic x(3).
       77  l   pic x(6) value all 'xy'.
       77  m   pic s9(12) comp value 123456789012.
       77  n   pic 9(4) value zeros.
       77  o   pic x(2) value spaces.
       77  p   unsigned-short value 65535.
       77  q   pic s9(6) comp-3 value 1.
       77  r   pic 9(5)v9(3) comp-3 value 98765.432.
       77  s   pic 99 value 7.
       procedure division.
           display 'a=' a '|b=' b '|c=' c '|d=' d '|e=' e '|f=' f '|'.
           display 'g=' g '|h=' h '|i=' i '|' 42 ' ' -1.50 ' ' zero.
           display 'j=' j '|k=' k '|l=' l '|m=' m '|n=' n '|o=' o '|'.
           display 'p=' p '|q=' q '|r=' r '|s=' s '|' quote space 'x'.
           display 'no' with no advancing.
           display 'adv'.
           goback.
