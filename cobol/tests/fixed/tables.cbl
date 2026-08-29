       identification division.
       program-id. tables.
      * groups, REDEFINES, OCCURS, subscripts, qualification, 88s.
       data division.
       working-storage section.
       01  month-list.
           05  filler  pic x(9) value 'January'.
           05  filler  pic x(9) value 'February'.
           05  filler  pic x(9) value 'March'.
       01  month-table redefines month-list.
           05  month-name pic x(9) occurs 3 times.
       01  grid.
           05  row occurs 2 times.
               10  cell occurs 3 times pic 99.
       01  i       pic 9 value 1.
       01  j       pic 9.
       01  k       pic s9(4) comp.
       01  rec-a.
           05  id      pic x(4) value 'AAAA'.
           05  amount  pic 9(3) value 7.
       01  rec-b.
           05  id      pic x(4) value 'BBBB'.
           05  amount  pic 9(3) value 42.
       01  flags.
           05  status-code pic x value 'N'.
               88  is-open      value 'O'.
               88  is-closed    value 'C' 'N'.
           05  grade      pic 99 value 55.
               88  passing    value 50 thru 100.
               88  honours    value 90 thru 100.
       01  whole   pic x(12) value 'abcdefghijkl'.
       01  parts redefines whole.
           05  left-part  pic x(4).
           05  mid-part   pic x(4).
           05  right-part pic x(4).
       01  stamp.
           05  s-year  pic 9999 value 2026.
           05  s-month pic 99 value 8.
           05  s-day   pic 99 value 29.
       01  stamp-num redefines stamp pic 9(8).
       procedure division.
       main-para.
           display month-name(1) '|' month-name(2) '|' month-name(3).
           move 2 to i.
           display month-name(i).
           move 3 to i.
           display month-name(i - 1).
           move 12 to cell(1, 1).
           move 34 to cell(1, 3).
           move 56 to cell(2, 2).
           move 0 to cell(1, 2) cell(2, 1) cell(2, 3).
           move 1 to i.
           move 3 to j.
           display 'cell(i,j)=' cell(i, j).
           display 'grid=' grid.
           display 'row2=' row(2).
           display id of rec-a ' ' amount of rec-a.
           display id of rec-b ' ' amount of rec-b.
           move rec-a to rec-b.
           display rec-b.
           display 'whole=' whole.
           display 'mid=' mid-part.
           move 'XYZ' to left-part.
           display 'whole=' whole.
           display 'stamp-num=' stamp-num.
           if is-closed display 'closed' end-if.
           if is-open display 'open' else display 'not open' end-if.
           set is-open to true.
           display 'status=' status-code.
           if is-open display 'now open' end-if.
           if passing display 'passing' end-if.
           if honours display 'honours' else display 'no honours' end-if.
           move 95 to grade.
           if honours and passing display 'top' end-if.
           if not honours display 'x' else display 'still top' end-if.
           move 1 to k.
           move 2 to i.
           move month-name(k) to whole.
           display whole.
           display cell(i, k).
           stop run.
