*> Constant entries (2002 13.9; 2023 13.10): 01 name CONSTANT [IS GLOBAL]
*> AS literal, arithmetic-expression, LENGTH OF or BYTE-LENGTH OF.  The
*> name stands wherever a literal of its class may (rule 2): a VALUE, an
*> OCCURS count, a PICTURE repetition, MOVE, DISPLAY, a relation, a
*> subscript.  GLOBAL reaches a contained program, and a constant that
*> is not GLOBAL leaves the name free there.  The X-COBOL survey
*> met eleven programs that use them (ISSUES 120).
identification division.
program-id. constent.
data division.
working-storage section.
01 max-keys      constant as 5.
01 width         constant as 2 * (max-keys + 1) - 4.
01 low-bound     constant as -2.5.
01 greeting      constant as "hello, constant".
01 tab-char      constant as x"09".
01 shared        constant is global as "seen from inside".
01 rec.
   05 rec-key    pic x(width) occurs max-keys.
   05 rec-amt    pic s9(3)v99 value low-bound.
01 rec-len       constant as length of rec.
01 key-bytes     constant as byte-length of rec-key(1).
01 k             pic 9 value max-keys.
01 shown         pic -9.99.
procedure division.
    display "max-keys " max-keys " width " width
    move greeting to rec-key(1)
    display "[" rec-key(1) "]"
    move low-bound to shown
    display "low-bound " shown " amt " rec-amt
    display "rec-len " rec-len " key-bytes " key-bytes
    if k = max-keys display "k = max-keys" end-if
    move "last" to rec-key(max-keys)
    display "[" rec-key(5) "]"
    display "tab[" tab-char "]"
    call "inner"
    stop run.

identification division.
program-id. inner.
data division.
working-storage section.
01 max-keys pic 99 value 42.
procedure division.
    display shared " " max-keys
    goback.
end program inner.
end program constent.
