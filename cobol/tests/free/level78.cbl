*> Level 78 constant-names, Micro Focus's (BP-E26; its VALUE clause,
*> format 3): a literal keeps its class; anything else is an integer,
*> computed strictly left to right (every operator one precedence,
*> parentheses first), with the bitwise AND, OR, EXCLUSIVE OR and NOT;
*> LENGTH OF a literal counts its digits or characters, of a data item
*> its storage.  A 78 may stand between a record's entries.  Fifteen
*> X-COBOL programs use them (ISSUES 120).  No oracle: GnuCOBOL has no
*> EXCLUSIVE OR and refuses LENGTH OF a numeric literal; without those two
*> lines it agrees under its -std=mf (checked 2026-10-01).
identification division.
program-id. level78.
data division.
working-storage section.
78 max-items     value 6.
78 title-text    value "level 78".
78 rate          value 2.75.
78 mixed         value 2 + 3 * 4.
78 grouped       value 2 + (3 * 4).
78 halved        value 7 / 2.
78 masks         value 12 and 10.
78 maskx         value 12 exclusive or 10.
78 masko         value 12 or 3.
78 lit-len       value length of "abcde".
78 num-len       value length of -012.50.
78 derived       value max-items * 2 - 1.
01 rec.
   05 item       pic x(3) occurs max-items.
78 inside-rec    value 99.
   05 tail       pic 9(3) value inside-rec.
78 rec-size      value length of rec.
01 amt           pic 9v99 value rate.
01 j             pic 99.
procedure division.
    display title-text " " max-items " " amt
    display "mixed " mixed " grouped " grouped " halved " halved
    display "and " masks " xor " maskx " or " masko
    display "lengths " lit-len " " num-len " rec " rec-size
    display "derived " derived " tail " tail
    perform varying j from 1 by 1 until j > max-items
        move j to item(j)
    end-perform
    display rec
    stop run.
