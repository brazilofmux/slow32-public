*> The 2014 behaviour changes that have a run-time face (2014 Annex E.2;
*> docs/conformance/edition-2014.md): item 7, CLOSE WITH NO REWIND of a
*> file not on unit media sets I-O status 07 (00 under -std=2002); item
*> 11, a value too near zero for a floating-point numeric-edited receiver
*> is EC-SIZE-TRUNCATION -- and a float that is not goes in whole, its
*> exponent kept (1.0E-20 went to zero before this audit); item 21, a
*> comma ending a PICTURE before VALUE is a separator; E.3 item 19, a
*> PICTURE of 63 characters.  The ec-size case is run by hand (fatal).
*> No oracle: GnuCOBOL 4 keeps status 00 for the CLOSE and refuses the
*> E picture's underflow case differently.
*> docs/conformance/edition-2014.md
identification division.
program-id. e2audit.
environment division.
input-output section.
file-control.
    select f assign to "e2audit.dat" organization sequential file status fs.
data division.
file section.
fd f.
01 frec pic x(10).
working-storage section.
01 fs pic xx.
01 fe pic +9.9e+99.
01 fl usage float-long value 1.5e-20.
01 fm usage float-long.
01 d pic 99, value zero.
01 p63 pic xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx value all "a".
procedure division.
    open output f write frec from "hello" close f display "close: " fs
    open input f close f with no rewind display "close with no rewind: " fs
    open input f close f reel display "close reel: " fs
    close f display "close: " fs
    move fl to fe display "move float: " fe
    compute fe = fl / 1.0e+10 display "compute float: " fe
    compute fm = fl * 1.0e+15 move fm to fe display "move 1.5e-5: " fe
    move 0.00000123 to fe display "move a decimal: " fe
    display "comma as a separator: " d
    display "63: " function length(p63) " " p63(60:4)
    stop run.
