identification division.
function-id. clamp.
*> A separately compiled function library for 2002/userfnx (cobol
*> ISSUES-50): its compile writes clamp.s32fn and label-of.s32fn, the
*> external repository the caller's compile reads.
data division.
linkage section.
01  v        pic s9(5).
01  lo       pic s9(5).
01  hi       pic s9(5).
01  c        pic s9(5).
procedure division using v lo hi returning c.
    evaluate true
        when v < lo move lo to c
        when v > hi move hi to c
        when other move v to c
    end-evaluate
    goback.
end function clamp.

identification division.
function-id. label-of.
data division.
linkage section.
01  code-in  pic 9.
01  lbl      pic x(6).
procedure division using code-in returning lbl.
    evaluate code-in
        when 1 move "one" to lbl
        when 2 move "two" to lbl
        when other move "many" to lbl
    end-evaluate
    goback.
end function label-of.
