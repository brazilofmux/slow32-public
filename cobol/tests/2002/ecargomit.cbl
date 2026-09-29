*> EC-PROGRAM-ARG-OMITTED (2023 14.9.4 GR 12): an omitted parameter
*> referenced -- not in the omitted-argument condition, which is how a
*> program asks -- raises the condition, fatal, after the declarative.
*> docs/conformance/call.md.  No oracle (the exception machinery is
*> GnuCOBOL's own).
identification division.
program-id. ecargomit.
data division.
working-storage section.
01 w pic x(4) value "wxyz".
procedure division.
    call "sub" using w
    call "sub" using omitted
    display "not reached"
    stop run.
end program ecargomit.

identification division.
program-id. sub.
data division.
linkage section.
01 p pic x(4).
procedure division using optional p.
declaratives.
dx section.
    use after exception condition ec-program-arg-omitted.
d1.
    display "declarative: " function exception-status.
end declaratives.
main section.
m1.
>>TURN EC-PROGRAM-ARG-OMITTED CHECKING ON
    if p is omitted display "sub: omitted" else display "sub: " p end-if
    display "sub: [" p "]"
    goback.
end program sub.
