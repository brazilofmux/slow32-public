*> Numeric fields of a screen ACCEPT, edited by the core (libcob/
*> scredit.h; docs/screen.md): a number is keyed as it is written, and
*> the field is the picture's editing of the digits after every key.
*>   a  ZZZ99.99   5 and Tab: 5.00 -- digits enter at the point
*>   b  ZZZ99.99   12.5: the point key goes to the fraction -- 12.50
*>   c  ZZ9 AUTO   three digits fill it and the cursor goes on
*>   d  -ZZ9.99    7 - . 2 5: the sign key, anywhere in the field
*>   e  9(3)V99    1 2 3 4 5: the integer part filling carries the
*>                 cursor over the assumed point -- 123.45
*>   f  ZZ9.99 BLANK WHEN ZERO, 3.5 in it: Ctrl-X clears, and the blank
*>                 shows when the field is left
*>   g  $$,$$9.99  1234.5 then Backspace three times: the fraction digit,
*>                 back onto the point, one integer digit -- 123.00
*>   h  9(5), 42 in it: Left moves onto the 2, 7 overtypes it -- 47
*> The keys come from scrnumed.keys.
*> No oracle: screens need a real tty.
identification division.
program-id. scrnumed.
data division.
working-storage section.
01  a pic 9(5)v99 value 0.
01  b pic 9(5)v99 value 0.
01  c pic 9(3) value 0.
01  d pic s9(3)v99 value 0.
01  e pic 9(3)v99 value 0.
01  f pic 9(3)v99 value 3.5.
01  g pic 9(4)v99 value 0.
01  h pic 9(5) value 42.
01  d-out pic -(4).99.
screen section.
01  s1.
    05  line 1 column 1 pic zzz99.99 using a.
    05  line 2 column 1 pic zzz99.99 using b.
    05  line 3 column 1 pic zz9 using c auto.
    05  line 4 column 1 pic -zz9.99 using d.
    05  line 5 column 1 pic 9(3)v99 using e.
    05  line 6 column 1 pic zz9.99 using f blank when zero.
    05  line 7 column 1 pic $$,$$9.99 using g.
    05  line 8 column 1 pic 9(5) using h.
procedure division.
    display s1
    accept s1
    move d to d-out
    display a at 1001 ' ' b ' ' c ' ' d-out ' ' e ' ' f ' ' g ' ' h
    stop run.
