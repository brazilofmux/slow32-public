identification division.
program-id. performdeep recursive.
*> PERFORMs under way, 1400 deep: every activation of a recursive
*> program leaves two ranges waiting -- a paragraph, and a THRU range
*> inside it -- while it calls itself, 700 times.  The runtime keeps a
*> frame for each on a stack that grows as it fills (256, 512, 1024),
*> and each must still say where to return when the calls come back.
*> No test filled it once: a fault in "the stack is full" passed them
*> all (docs/performance.md, 2026-10-02).
data division.
working-storage section.
01 depth pic 9(4) value 0.
01 deepest pic 9(4) value 0.
01 returned pic 9(4) value 0.
01 inner-done pic 9(4) value 0.
local-storage section.
01 me pic 9(4).
procedure division.
m1.
    add 1 to depth
    move depth to me
    perform one-level
    if me = 1
        display "deepest " deepest
        display "paragraphs returned " returned
        display "ranges returned " inner-done
    end-if
    goback.
one-level.
    perform inner thru inner-exit
    add 1 to inner-done
    add 1 to returned.
inner.
    if depth > deepest
        move depth to deepest
    end-if
    if me < 700
        call "performdeep"
    end-if.
inner-exit.
    exit.
