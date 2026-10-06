*> Screen clauses on the 01 and the attributes that were read and
*> dropped (2023 13.17.2 format 1, 13.18.6, 13.18.9):
*>   - AUTO and REQUIRED written on the 01 reach the input entries
*>     below it (a display attribute there reaches every entry);
*>   - BELL sounds once at the start of a DISPLAY (BEL, byte 7, in the
*>     stream), not for the ACCEPT that follows;
*>   - BLINK is painted as SGR 5 (one attribute is painted per field:
*>     REVERSE-VIDEO, UNDERLINE, HIGHLIGHT, LOWLIGHT win over it, in that
*>     order -- docs/conformance/screen.md);
*>   - REQUIRED on an item that takes no input is allowed and does
*>     nothing (13.18.47.3 rule 1): it used to be refused.
*> Keys: two digits fill the AUTO field and the ACCEPT ends.  The keys
*> come from scrattr.keys.
*> No oracle: screens need a real tty.
identification division.
program-id. scrattr.
data division.
working-storage section.
01  a pic 99 value 0.
screen section.
01  s1 auto required.
    05  line 1 col 1 value 'beep' bell.
    05  line 2 col 1 value 'blinking' blink required.
    05  line 3 col 1 pic 99 using a.
procedure division.
    display s1
    accept s1
    display a at 0501
    stop run.
