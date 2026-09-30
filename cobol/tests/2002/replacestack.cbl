identification division.
program-id. replacestack.
*> REPLACE as COBOL 2002 has it (14.x; 2023 7.2.4): anywhere a space
*> precedes it, not only after a period; ALSO queues the active statement
*> and adds its operands after its own; LAST OFF brings back the one
*> queued; OFF ends them all.  LEADING and TRAILING replace part of a
*> word, in REPLACE and in COPY ... REPLACING.
data division.
working-storage section.
copy cpfx replacing leading ==pfx== by ==ws==.
01 aa pic x(3) value "aaa".
01 bb pic x(3) value "bbb".
01 cc pic x(3) value "ccc".
01 ws-first  pic x(3) value "one".
01 zz-item   pic x(3) value "zzz".
procedure division.
    display ws-a " " ws-b
    display aa replace ==xx== by ==bb==.
    display xx
    replace also ==yy== by ==cc==.
    display xx " " yy
    replace last off.
    display xx
    replace leading ==qq== by ==ws== trailing ==-end== by ==-item==.
    display qq-first " " zz-end
    replace off.
    stop run.
end program replacestack.
