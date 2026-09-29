       identification division.
       program-id. valrul.
      * The VALUE clause's rules (X3.23-1985 5.15.3-5.15.6; 2023
      * 13.18.63.3): a numeric item takes a numeric literal or ZERO, a
      * signed literal only if it is signed, no nonzero digit lost; a
      * group's literal fits it, nothing below it has a VALUE, and
      * nothing below it is JUSTIFIED, SYNCHRONIZED or not DISPLAY; a
      * condition-name's literals fit its variable and THRU runs upward;
      * in the FILE SECTION only condition-names take a VALUE.
       environment division.
       input-output section.
       file-control.
           select f assign to "x.dat".
       data division.
       file section.
       fd f.
       01 fr pic x(4) value "abcd".
       working-storage section.
       01 n1 pic 9(3) value spaces.
       01 n2 pic 9(2) value -1.
       01 n3 pic 9(2) value 1.5.
       01 g1 value "abcdef".
          05 g1a pic x(2).
       01 g2 value "ab".
          05 g2a pic x(2) value "cd".
       01 g3 value zeros.
          05 g3a pic 9(4) comp.
       01 c pic 9.
          88 c1 value 5 thru 3.
          88 c2 value "x".
          88 c3 value 12.
       01 d pic x.
          88 d1 value "xy".
       procedure division.
           stop run.
