*> SET pointer TO NULL and TO another pointer, a relation with NULL,
*> INITIALIZE of a pointer (13.18.60.3 rule 9): SET ... TO NULL failed
*> with "null cannot be moved to the pointer item" before the USAGE
*> sweep found it.  ADDRESS OF, which would point one at an item, is
*> not implemented, so the pointers here start NULL.
*> docs/conformance/usage.md
identification division.
program-id. pointerset.
data division.
working-storage section.
01 p usage pointer.
01 q usage pointer.
procedure division.
    set p to null
    if p = null display "null" else display "set" end-if
    set q to p
    if q = p display "equal" end-if
    initialize q
    if q = null display "initialized to null" else display "initialize left it" end-if
    stop run.
