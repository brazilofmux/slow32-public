identification division.
program-id. ecptrnull.
*> EC-DATA-PTR-NULL (2002 13.16.5 general rule 3): a BASED item
*> referenced while its address is NULL.  Checked, the MOVE raises the
*> condition before touching storage, and the run ends after the
*> declarative (the condition is fatal).  ADDRESS OF a NULL based
*> record is not such a reference: it is the NULL itself.
*> docs/conformance/usage.md.  No oracle (the exception machinery is
*> GnuCOBOL's own).
data division.
working-storage section.
01  w        pic x(4) value "wxyz".
01  b        pic x(4) based.
01  p        usage pointer.
procedure division.
declaratives.
nd section.
    use after exception condition ec-data-ptr-null.
n1.
    display "declarative: " function exception-status.
end declaratives.
main section.
m1.
>>TURN EC-DATA-PTR-NULL CHECKING ON
    set p to address of b
    if p = null display "address of a null based record is null" end-if
    set address of b to address of w
    display "based over w: " b
    set address of b to null
    move "q" to b
    display "not reached"
    stop run.
