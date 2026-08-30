*> The screen as docs/screen.md's "eventual target" has it: numeric
*> fields edited on the decimal point (Z9.99 USING, 99 USING AUTO),
*> a text field edited in place with the cursor keys, SECURE echoing
*> stars, REQUIRED refusing to leave an empty field, UNDERLINE and
*> colours painted, LINE PLUS / COLUMN PLUS placing slots.  The keys
*> come from screen2.keys; the ANSI stream is the expected output.
*> No oracle: GnuCOBOL's screens need a real tty.
identification division.
program-id. screen2.
data division.
working-storage section.
77  amount            pic s9(3)v99 value 12.5.
77  month-in          pic 99 value 0.
77  who               pic x(8) value 'joe     '.
77  secret            pic x(4).
77  must              pic x(3).
screen section.
01  form-in.
    05  blank screen.
    05  line 2 column 10 value 'Amount' underline.
    05  line 2 column 20 pic z9.99 using amount.
    05  line plus 1 column 10 value 'Month' foreground-color 4.
    05  column 20 pic 99 using month-in auto.
    05  line 4 column 10 value 'Who' background-color 1.
    05  column plus 7 pic x(8) using who.
    05  line 5 column 10 value 'Secret'.
    05  line 5 column 20 pic x(4) to secret secure.
    05  line 6 column 10 value 'Must'.
    05  line 6 column 20 pic x(3) to must required.
procedure division.
    accept form-in.
    display 'amount=' amount ' month=' month-in ' who=[' who
            '] secret=[' secret '] must=[' must ']'.
    stop run.
end program screen2.
