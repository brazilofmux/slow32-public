       identification division.
       program-id. inspop.
      * INSPECT's operand rules (X3.23-1985 INSPECT syntax rules 1-3 and
      * 7; 2023 14.9.22.3): the inspected item is a group or a DISPLAY
      * elementary item; the other operands are elementary DISPLAY items
      * or nonnumeric literals, never an ALL figurative constant; the
      * CHARACTERS BY operand is one character.
       data division.
       working-storage section.
       01 s pic x(10).
       01 k pic 9(4) comp.
       01 t pic 99.
       01 g.
          05 g1 pic x.
       procedure division.
           inspect k tallying t for all "1".
           inspect s tallying t for all k.
           inspect s tallying t for all g.
           inspect s tallying t for all all "ab".
           inspect s tallying t for all 12.
           inspect s replacing characters by "ab".
           stop run.
