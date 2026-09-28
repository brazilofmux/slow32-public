identification division.
program-id. ecrefmod.
*> EC-BOUND-REF-MOD (COBOL 2002 8.4.2.4; cobol ISSUES-56): a reference
*> modification with computed positions leaving the item.  s(p:l) with
*> p = 8, l = 3 fits; with l = 4 it runs past the tenth character.
*> Fatal.  No oracle (ecraise).
data division.
working-storage section.
01  s        pic x(10) value "abcdefghij".
01  w        pic x(10).
01  p        pic 99 value 8.
01  l        pic 99 value 3.
procedure division.
declaratives.
rm-decl section.
    use after exception condition ec-bound-ref-mod.
r1.
    display "declarative: " function exception-status.
end declaratives.
main section.
m1.
>>TURN EC-BOUND-REF-MOD CHECKING ON
    move s(p:l) to w
    display "s(8:3) = [" w "]"
    move s(p:) to w
    display "s(8:) = [" w "]"
    move 4 to l
    move s(p:l) to w
    display "not reached"
    stop run.
end program ecrefmod.
