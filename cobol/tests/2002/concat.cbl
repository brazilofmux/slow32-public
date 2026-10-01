*> Concatenation expressions (2002 and 2023 8.8.3): literal & literal is
*> one literal of the operands' class, usable wherever such a literal is
*> (general rule 3) -- a VALUE, a MOVE, a DISPLAY, a relation, a constant
*> entry; chained, and across lines; SPACE and QUOTE taking the other
*> operand's class (rule 1a).  ZERO and national operands are in
*> 2002/concatfig, which GnuCOBOL refuses.  The X-COBOL survey met four
*> programs writing it ('test.db' & x'00').
identification division.
program-id. concat.
data division.
working-storage section.
01 dbname   pic x(8) value 'test.db' & x'00'.
01 joined   constant as "ab" & "cd" &
                        "ef".
01 w        pic x(12).
01 i        pic 99.
procedure division.
    display joined
    move "left" & space & "right" to w
    display "[" w "]"
    display quote & "q" & quote
    move function ord(dbname(8:1)) to i
    display "[" dbname(1:7) "] last byte's ordinal " i
    if w = "left" & " right" display "relation holds" end-if
    stop run.
