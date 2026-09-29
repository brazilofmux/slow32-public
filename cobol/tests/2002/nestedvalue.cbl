*> A contained program with BY VALUE parameters -- which enlarge its own
*> stack frame -- inside a program that has none: the outer program's exit
*> must restore its own frame, not the contained one's.  The contained
*> program is compiled before the outer one's exit code is emitted.
*> docs/conformance/call.md
identification division.
program-id. nestedvalue.
data division.
working-storage section.
01 r binary-long value 0.
01 k binary-long value 21.
procedure division.
    call "twice" using by value k by reference r
    display "twice(21) = " r
    call "twice" using by value 50 by reference r
    display "twice(50) = " r
    goback.

identification division.
program-id. twice.
data division.
linkage section.
01 n   binary-long.
01 out binary-long.
procedure division using by value n by reference out.
    compute out = n * 2
    goback.
end program twice.
end program nestedvalue.
