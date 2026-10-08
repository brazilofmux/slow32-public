*> The cursor locator, the endings ON EXCEPTION takes, and status 8000
*> (2023 9.2.3, 9.2.5, 14.9.1.4 rules 18 and 23-25).
*>   1  CURSOR IS names a six-character item: line 3 column 7 is in the
*>      second field, at its third position, so the cursor starts there
*>      and Z overtypes the c.  Enter: NOT ON EXCEPTION, status 0000, and
*>      the locator is where the cursor stood (one to the right).
*>   2  a locator that is in no input field: the first field, as if there
*>      were no CURSOR clause.  F2 ends it: ON EXCEPTION, status 1002.
*>   3  a screen with nothing to accept into (only BLANK SCREEN: one with
*>      FROM or VALUE items and no input is refused, 14.9.1.3 rule 4):
*>      unsuccessful, 8000, ON EXCEPTION, and no key is waited for.
*> The keys come from scrcursor.keys.
*> No oracle: screens need a real tty.
identification division.
program-id. scrcursor.
environment division.
configuration section.
special-names.
    cursor is cur-pos
    crt status is crt.
data division.
working-storage section.
01  cur-pos pic 9(6) value 003007.
01  crt pic x(4).
01  a pic x(5) value 'first'.
01  b pic x(5) value 'abcde'.
01  how pic x(4).
screen section.
01  s1.
    05  line 2 column 5 pic x(5) using a.
    05  line 3 column 5 pic x(5) using b.
01  s2.
    05  blank screen.
procedure division.
    display s1
    accept s1
        on exception move 'exc' to how
        not on exception move 'ok' to how
    end-accept
    display how at 0601 ' ' crt ' ' cur-pos ' [' a '] [' b ']'
    move 020099 to cur-pos
    accept s1
        on exception move 'exc' to how
        not on exception move 'ok' to how
    end-accept
    display how at 0701 ' ' crt ' ' cur-pos ' [' a '] [' b ']'
    accept s2
        on exception move 'exc' to how
        not on exception move 'ok' to how
    end-accept
    display how at 0801 ' ' crt
    stop run.
