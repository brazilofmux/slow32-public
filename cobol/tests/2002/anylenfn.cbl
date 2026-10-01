*> ANY LENGTH parameters of user-defined functions (2023 13.18.2.3 rules
*> 2 and 4): an item of any length by reference, a literal by content in
*> a copy of its own length, and a recursive function whose every
*> activation keeps its own length (each a shorter part of its
*> caller's), the length saved and restored with the activation.
*> No oracle: GnuCOBOL 4.0-early-dev dies with SIGSEGV in count-a.
identification division.
function-id. count-a.
data division.
working-storage section.
01 n binary-long.
linkage section.
01 l-s pic x any length.
01 l-r pic 9(4).
procedure division using l-s returning l-r.
    move 0 to n
    inspect l-s tallying n for all "a"
    move n to l-r
    goback.
end function count-a.

identification division.
function-id. rcount.
*> recursive: each activation's l-s is a shorter part of its caller's
data division.
linkage section.
01 l-s pic x any length.
01 l-r pic 9(4).
procedure division using l-s returning l-r.
    if function length(l-s) = 1
        if l-s = "a" move 1 to l-r else move 0 to l-r end-if
    else
        move function rcount(l-s(2:)) to l-r
        if l-s(1:1) = "a" add 1 to l-r end-if
    end-if
    goback.
end function rcount.

identification division.
program-id. anylenfn.
environment division.
configuration section.
repository.
    function count-a
    function rcount.
data division.
working-storage section.
01 long-s  pic x(20) value "banana and bandanas".
01 r       pic 9(4).
procedure division.
    move count-a(long-s) to r
    display "count-a " r
    move count-a("aaa") to r
    display "count-a literal " r
    move rcount(long-s) to r
    display "rcount " r
    move rcount(long-s(1:6)) to r
    display "rcount of a part " r
    stop run.
end program anylenfn.
