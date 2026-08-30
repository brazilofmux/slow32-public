*> What menu and taskdt drag in: EVALUATE in its forms, INSPECT
*> TALLYING and REPLACING, INITIALIZE, reference modification with
*> arithmetic, FUNCTION LENGTH -- and taskdt's date-string build with
*> a fixed timestamp in place of CURRENT-DATE.
identification division.
program-id. nucleus2.
data division.
working-storage section.
01  option      pic xx.
01  menu-to-show pic x value 'M'.
01  n           pic s9(4) comp.
01  grade       pic 99.
01  txt         pic x(12) value 'a00b00c00'.
01  cnt         pic 99 comp.
01  cnt2        pic 99 comp.
01  rec.
    05  r-name  pic x(8) value 'preset'.
    05  r-num   pic 9(4) value 42.
    05  r-bin   pic s9(4) comp value -7.
    05  r-tab occurs 3 times.
        10  r-a pic xx value 'zz'.
        10  r-b pic 99 value 9.
01  s           pic x(10) value 'abcdefghij'.
01  i           pic 99 comp value 3.
01  len         pic 99 comp.
01  todays-day-of-month  pic 9(2).
01  todays-year          pic 9(5).
01  todays-date          pic x(30).
01  leading-zeros        pic 99 comp.
01  output-index         pic 99 comp.
01  time-stamp.
    05  ts-date.
        10  ts-year         pic 9999.
        10  ts-month        pic 99.
        10  ts-dom          pic 99.
    05  ts-rest             pic x(13).
01  month-list.
    05  filler pic x(10) value 'January'.
    05  filler pic x(10) value 'February'.
    05  filler pic x(10) value 'March'.
01  month-table redefines month-list.
    05  name-of-month pic x(10) occurs 3 times.
01  m           pic 99 comp.
procedure division.
*> EVALUATE
    move 'DT' to option.
    perform show-option.
    move 'fm' to option.
    perform show-option.
    move 'xx' to option.
    perform show-option.
    move 'M' to menu-to-show.
    move 'xx' to option.
    perform show-option.
    move 75 to grade.
    evaluate true
        when grade < 50 display 'fail'
        when grade < 70 display 'pass'
        when grade < 90 display 'merit'
        when other display 'distinction'
    end-evaluate.
    move 5 to n.
    evaluate n
        when 1 thru 3 display 'low'
        when 4 when 5 display 'mid'
        when not 6 display 'x'
    end-evaluate.
    evaluate n also grade
        when 5 also 75 display 'both'
        when 5 also any display 'first only'
    end-evaluate.
    evaluate n + 1
        when 6 display 'six'
    end-evaluate.
*> INSPECT
    move 0 to cnt cnt2.
    inspect txt tallying cnt for all '0' cnt2 for leading 'a'.
    display 'zeros=' cnt ' leading-a=' cnt2.
    move 0 to cnt.
    inspect txt tallying cnt for characters.
    display 'chars=' cnt.
    inspect txt replacing all '00' by '--' first 'a' by 'A'.
    display '[' txt ']'.
    inspect txt replacing characters by '.'.
    display '[' txt ']'.
*> INITIALIZE
    initialize rec.
    display '[' r-name '][' r-num '][' r-bin '][' r-a(1) r-b(2) ']'.
    move 'xy' to r-a(3).
    initialize r-tab(3).
    display '[' r-a(3) '][' r-b(3) ']'.
*> reference modification
    display '[' s(2:3) '][' s(8:) ']'.
    display '[' s(i:2) '][' s(i + 1:i) ']'.
    move 'XY' to s(4:2).
    display s.
    move s(1:3) to option.
    display option.
    move function length(s) to len.
    display len ' ' function length('hello') ' ' function length(rec).
    move 'abc' to s(1:).
    display '[' s ']'.
    if s(4:1) = ' ' display 'padded' end-if.
*> taskdt's date string, from a fixed stamp
    move '2026020917015141-0700' to time-stamp.
    move 1 to output-index.
    move spaces to todays-date.
    move ts-month to m.
    string name-of-month(m) delimited by space
           ', ' delimited by size
           into todays-date with pointer output-index.
    initialize leading-zeros.
    move ts-dom to todays-day-of-month.
    inspect todays-day-of-month tallying leading-zeros for leading zero.
    string todays-day-of-month(leading-zeros + 1:function length(todays-day-of-month) - leading-zeros)
           ', ' delimited by size
           into todays-date with pointer output-index.
    initialize leading-zeros.
    move ts-year to todays-year.
    inspect todays-year tallying leading-zeros for leading zero.
    string todays-year(leading-zeros + 1:function length(todays-year) - leading-zeros)
           delimited by size
           into todays-date with pointer output-index.
    display '[' todays-date ']'.
    stop run.
show-option.
    move function upper-case(option) to option.
    evaluate option
    when 'DT'
        display 'date'
    when 'DW'
        move 'D' to menu-to-show
    when 'FM'
        move 'F' to menu-to-show
        display 'file'
    when other
        evaluate menu-to-show
        when 'D' display 'daily'
        when 'F' display 'files'
        when 'M' display 'main'
        end-evaluate
    end-evaluate.
