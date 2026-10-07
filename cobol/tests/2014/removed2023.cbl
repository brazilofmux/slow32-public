*> What COBOL 2023 removed (its Annex E.2 item 1), the language through
*> 2014 and taken under -std=2014 as class R behaviour points (BP-R1 to
*> BP-R5, docs/behavior-points.md); under -std=2023 each is refused
*> (tests/bad/std2023-*).  Here: CALL ... ON OVERFLOW (BP-R2), CLOSE WITH
*> LOCK and its status 38 (BP-R3), a COPY REPLACING operand that is a
*> word (BP-R4; copy/removed2023.cpy), EXIT FUNCTION (BP-R5).  The word
*> continuation (BP-R1) is fixed form's: fixed/wordcont.  No oracle:
*> GnuCOBOL 4 has no EXIT FUNCTION.  docs/conformance/edition-2023.md
identification division.
function-id. twice.
data division.
linkage section.
01 n pic 9(3).
01 r pic 9(4).
procedure division using n returning r.
    compute r = n * 2
    exit function.
end function twice.
identification division.
program-id. removed2023.
environment division.
configuration section.
repository.
    function twice.
input-output section.
file-control.
    select f assign to "removed2023.dat" organization sequential file status fs.
data division.
file section.
fd f.
01 frec pic x(10).
working-storage section.
01 fs pic xx.
copy "removed2023.cpy" replacing TAG by my-rec LEN by 5.
procedure division.
    display twice(21)
    open output f close f with lock display "with lock: " fs
    open input f display "open after lock: " fs
    call "nobody" on overflow display "overflow" end-call
    move "hello" to my-rec display my-rec
    stop run.
end program removed2023.
