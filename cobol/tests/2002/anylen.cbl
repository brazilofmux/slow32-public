*> ANY LENGTH (2002; 2023 13.18.2): a parameter whose length is its
*> argument's (general rule 1b), in contained programs (rule 2), passed
*> on from one to another, received into by MOVE (the caller's item
*> changes), reference-modified with literal and computed starts, counted
*> by LENGTH and INSPECT, compared.  Functions are 2002/anylenfn, a
*> national parameter 2002/anylennat.  Thirteen X-COBOL programs use it
*> (ISSUES 120).
identification division.
program-id. anylen.
data division.
working-storage section.
01 short-s pic x(5)  value "a b a".
01 long-s  pic x(20) value "banana and bandanas".
procedure division.
    call "shout" using short-s
    call "shout" using long-s
    display "back: [" short-s "]"
    stop run.

identification division.
program-id. shout.
data division.
working-storage section.
01 k binary-long.
linkage section.
01 l-text pic x any length.
procedure division using l-text.
    display "[" l-text "] " function length(l-text) " " l-text(2:3) " " l-text(function length(l-text):)
    call "inner" using l-text
    move 0 to k
    inspect l-text tallying k for all " "
    display "  spaces " k
    if l-text(1:1) = "a" move "A" to l-text(1:1) end-if
    if function length(l-text) = 5 move "XYZ" to l-text end-if
    goback.
end program shout.

identification division.
program-id. inner is common.
data division.
linkage section.
01 l-in pic x any length.
procedure division using l-in.
    display "  inner sees " function length(l-in)
    goback.
end program inner.

end program anylen.
