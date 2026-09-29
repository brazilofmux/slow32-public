identification division.
program-id. allocfree.
*> ALLOCATE and FREE (2002 14.8.3, 14.8.14): storage for a BASED record,
*> and a number of characters RETURNING a pointer; a second based record
*> laid over it; FREE sets the pointer NULL; FREE of NULL does nothing;
*> 0 characters give NULL; FREE of storage ALLOCATE did not obtain is
*> EC-STORAGE-NOT-ALLOC (nonfatal), the pointer left as it was.
*> docs/conformance/usage.md.  No oracle (the exception machinery is
*> GnuCOBOL's own).
data division.
working-storage section.
01  w        pic x(4) value "wxyz".
01  rec      based.
    05 r-id  pic 9(4).
    05 r-nm  pic x(6).
01  buf      pic x(12) based.
01  p        usage pointer.
01  q        usage pointer.
01  n        pic 99 value 12.
procedure division.
declaratives.
st section.
    use after exception condition ec-storage.
s1.
    display "declarative: " function exception-status.
end declaratives.
main section.
m1.
>>TURN EC-STORAGE CHECKING ON
    allocate rec returning q
    move 42 to r-id  move "widget" to r-nm
    display "rec: " r-id " " r-nm
    if address of rec = q display "returned its address" end-if
    allocate n characters initialized returning p
    set address of buf to p
    if buf = low-values display "twelve zero bytes" end-if
    move "hello, world" to buf
    display "buf: " buf
    free p
    if p = null display "p freed to null" end-if
    free p
    display "free of null: nothing"
    allocate 0 characters returning p
    if p = null display "0 characters: null" end-if
    set p to address of w
    free p
    if p = address of w display "not freed, unchanged" end-if
    free q
    if q = null display "q freed" end-if
    stop run.
