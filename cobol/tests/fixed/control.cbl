       identification division.
       program-id. control.
      * IF, conditions, every PERFORM form, GO TO, sections and the
      * PERFORM stack.
       data division.
       working-storage section.
       01  i       pic 99 value 0.
       01  j       pic 99.
       01  n       pic s9(4) comp value 3.
       01  total   pic s9(6) comp value 0.
       01  code-x  pic x value 'b'.
       01  txt     pic x(5) value 'ab12 '.
       01  num     pic 9(3) value 42.
       01  sn      pic s9(3) value -5.
       01  tbl.
           05  e occurs 5 times pic 99.
       procedure division.
       main section.
       start-here.
           perform show-i 3 times.
           perform count-up until i > 6.
           display 'i=' i.
           perform varying i from 1 by 1 until i > 5
               move i to e(i)
           end-perform.
           display tbl.
           perform varying i from 1 by 2 until i > 5
               after j from 3 by -1 until j < 1
               display i j ' ' with no advancing
           end-perform.
           display '.'.
           move 0 to i.
           perform with test after until i >= 3
               add 1 to i
           end-perform.
           display 'after: ' i.
           move 0 to i.
           perform test-after-para with test after until i >= 3.
           display 'after para: ' i.
           perform n times
               add 10 to total
           end-perform.
           display 'total=' total.
           perform first-para thru last-para.
           display 'back'.
           perform nested-outer.
           if code-x = 'a' display 'a'
           else if code-x = 'b' display 'b'
                else display 'other' end-if
           end-if.
           if txt = 'ab12' display 'refmod' end-if.
           if num > 40 and num < 50 display 'in range' end-if.
           if num = 42 or sn = 0 display 'either' end-if.
           if not (num = 42) display 'not' else display 'is' end-if.
           if sn is negative display 'neg' end-if.
           if sn not positive display 'not pos' end-if.
           if num is numeric display 'numeric' end-if.
           if txt is alphabetic display 'alpha' else display 'not alpha' end-if.
           if code-x is alphabetic-lower display 'lower' end-if.
           if num not = 41 display 'ne' end-if.
           if num greater than 41 display 'gt' end-if.
           if num is less than or equal to 42 display 'le' end-if.
           if num <> 42 display 'x' else display 'eq' end-if.
           if sn < num display 'lt' end-if.
           if 'ab' = txt display 'x' else display 'str ne' end-if.
           if txt = 'ab12' display 'padded eq' end-if.
           if num = 42 next sentence else display 'x'.
           display 'sentence end'.
           move 2 to i.
           go to pick-1 pick-2 pick-3 depending on i.
           display 'fell through'.
       pick-1.
           display 'one'.
           go to done.
       pick-2.
           display 'two'.
           go to done.
       pick-3.
           display 'three'.
       done.
           stop run.
       show-i.
           display 'show ' i.
       count-up.
           add 1 to i.
       test-after-para.
           add 1 to i.
       subs section.
       first-para.
           display 'first'.
       middle-para.
           display 'middle'.
           perform inner-para.
       last-para.
           display 'last'.
       inner-para.
           display 'inner'.
       nested-outer.
           display 'outer in'.
           perform nested-inner.
           display 'outer out'.
       nested-inner.
           display 'inner in'.
           perform show-i 2 times.
           display 'inner out'.
