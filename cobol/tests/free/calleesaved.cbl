identification division.
program-id. calleesaved.
*> A COBOL program called from C must give back the callee-saved
*> registers (the SLOW-32 C ABI: r11-r28; cobol ISSUES-57).  C holds
*> values in them across a call into CSVSUB, whose dynamic CALL uses
*> r12 and r13; the prologue used to save only r11, and C's values came
*> back wrong.  "default dialect": GOBACK and the C bridge.
data division.
working-storage section.
01  ok       pic s9(9) comp-5.
procedure division.
    call "csvdrive" returning ok
    if ok = 1 display "callee-saved registers preserved"
    else display "callee-saved registers CLOBBERED" end-if
    stop run.
end program calleesaved.
