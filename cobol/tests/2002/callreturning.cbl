*> PROCEDURE DIVISION RETURNING for a program (2023 14.2.2 rules 4-6,
*> 14.8.3, 14.9.4 GR 4): the caller's item receives the result, an
*> alphanumeric one and a numeric one; a CALL without RETURNING leaves
*> the caller's items alone.  docs/conformance/call.md.  No oracle:
*> GnuCOBOL 4 does not implement program RETURNING ("-Wpending").
identification division.
program-id. callreturning.
data division.
working-storage section.
01 greeting pic x(12) value spaces.
01 total    pic s9(7)v99 value 0.
01 a        pic s9(5)v99 value 123.45.
01 b        pic s9(5)v99 value 76.55.
procedure division.
    call "greet" using by content "world" returning greeting
    display "[" greeting "]"
    call "addup" using a b returning total
    display total
    move 1 to a
    call "addup" using a b
    display total
    call "addup" using a b returning total
    display total
    stop run.
end program callreturning.

identification division.
program-id. greet.
data division.
linkage section.
01 who    pic x(5).
01 result pic x(12).
procedure division using who returning result.
    string "hello " who delimited by size into result
    goback.
end program greet.

identification division.
program-id. addup.
data division.
linkage section.
01 x      pic s9(5)v99.
01 y      pic s9(5)v99.
01 s      pic s9(7)v99.
procedure division using x y returning s.
    compute s = x + y
    goback.
end program addup.
