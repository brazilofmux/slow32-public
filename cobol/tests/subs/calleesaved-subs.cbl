identification division.
program-id. csvsub.
*> Called from C (tests/c/calleesaved.c): a dynamic CALL, whose code
*> uses r12 and r13 -- and in-line loops four deep over binary items,
*> which the compiler keeps in r14 to r17 while the loops run
*> (src/cobc/loopreg.h): the entry saves the ones used.
data division.
working-storage section.
01  target   pic x(8) value "csvnoop".
01  i        pic 9(4) comp.
01  j        pic 9(4) comp.
01  k        pic 9(4) comp.
01  l        pic 9(4) comp.
01  total    pic 9(9) comp value 0.
procedure division.
    call target
    perform varying i from 1 by 1 until i > 3
        perform varying j from 1 by 1 until j > 3
            perform varying k from 1 by 1 until k > 2
                perform varying l from 1 by 1 until l > 2
                    compute total = total + i + j + k + l
                end-perform
            end-perform
        end-perform
    end-perform
    if total not = 252 display "csvsub: the loops count " total end-if
    goback.
end program csvsub.

identification division.
program-id. csvnoop.
procedure division.
    goback.
end program csvnoop.
