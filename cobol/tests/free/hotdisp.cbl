*> hotdisp -- GitHub #29 shape (3): unsigned DISPLAY integers on the inline
*> arithmetic path.  The receiver is decoded, added to and re-encoded in
*> registers instead of going through cob_top_addto's cob_get_num plus
*> cob_put_num pair.  Everything cob_put_num_x would have done to the value
*> still has to happen, and this is where it is checked: truncation to the
*> picture's digits (a DISPLAY item is exactly its digits, with none of the
*> slack a binary field has), and an unsigned receiver taking the magnitude
*> rather than a negative.  Plus PERFORM VARYING on a DISPLAY index, which
*> reaches the same path.
identification division.
program-id. hotdisp.
data division.
working-storage section.
01  a3   pic 9(3) value 0.
01  a7   pic 9(7) value 0.
01  a9   pic 9(9) value 0.
01  g    pic 9(5) value 0.
01  idx  pic 9(4) value 0.
01  acc  pic 9(6) value 0.
procedure division.
main-procedure.
    move 999 to a3
    add 1 to a3
    display 'A ' a3
    move 998 to a3
    add 5 to a3
    display 'B ' a3
    move 3 to a3
    subtract 5 from a3
    display 'C ' a3
    move 1234567 to a7
    add 1 to a7
    display 'D ' a7
    move 999999999 to a9
    add 1 to a9
    display 'E ' a9
    add 7 to 5 giving g
    display 'F ' g
    move 0 to acc
    perform varying idx from 1 by 1 until idx > 10
        add idx to acc
    end-perform
    display 'G ' acc ' ' idx
    move 100 to a3
    subtract 1 from a3
    display 'H ' a3
    move 0 to a3
    subtract 1 from a3
    display 'I ' a3
    stop run.
end program hotdisp.
