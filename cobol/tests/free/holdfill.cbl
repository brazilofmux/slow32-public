*> A MOVE SPACES (or ZEROS) held back by the lowering until a path needs
*> it (lower.h pf_*; performance.md 2026-10-08): dead under a move into
*> the whole item, emitted where a path reads the item first, changes
*> its subscript, loops, moves into a part or another element, or
*> leaves an IF arm without covering it.  Every case runs inside an
*> in-line loop so the statements form an island; the output must be
*> what the fills as written give (-fno-hir, and the oracle).
identification division.
program-id. holdfill.
data division.
working-storage section.
01 i pic 9.
01 j pic 9.
01 k pic 9.
01 n pic 9.
01 flag pic x.
01 x1 pic x(8).
01 x2 pic x(8).
01 x3 pic x(8).
01 x4 pic x(8).
01 z1 pic x(6).
01 z2 pic x(6).
01 tab.
   05 t pic x(5) occurs 4.
01 short pic xx value "ab".
01 long pic x(6) value "ABCDEF".
procedure division.
    perform varying i from 1 by 1 until i > 2
        if i = 1 move "y" to flag else move "n" to flag end-if
        move 1 to k
*>      1. dead: the move covers the whole item
        move spaces to x1
        move short to x1
*>      2. one arm covers, the other does not
        move spaces to x2
        if flag = "y"
            move long to x2
        end-if
*>      3. read in between: the display must show spaces
        move all "*" to x3
        move spaces to x3
        display "[" x3 "]"
        move short to x3
*>      4. the subscript changes: the fill lands on the old element
        move all "#" to t(1) t(2) t(3) t(4)
        move spaces to t(k)
        add 1 to k
        move short to t(k)
*>      5. zeros then a shorter move: the tail must stay zeros
        move zeros to z1
        move short to z1
*>      6. zeros then a sender as long: dead
        move zeros to z2
        move long to z2
*>      7. a part, not the whole: the fill stands
        move all "*" to x4
        move spaces to x4
        move "abc" to x4(2:3)
*>      8. nested arms: both cover; one of three paths does not
        move spaces to t(3)
        if flag = "y"
            if i = 1 move short to t(3) else move long to t(3) end-if
        else
            move 7 to n
        end-if
        display "[" x1 "][" x2 "][" x3 "][" x4 "][" z1 "][" z2 "]"
        display "[" t(1) "][" t(2) "][" t(3) "][" t(4) "]"
    end-perform
*>  9. a fill followed by a loop that does not touch the item, then the move
    move all "*" to x1
    move spaces to x1
    perform varying j from 1 by 1 until j > 3
        add 1 to n
    end-perform
    move short to x1
    display "[" x1 "] " n
    stop run.
