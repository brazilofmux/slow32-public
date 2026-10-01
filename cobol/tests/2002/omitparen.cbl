*> The omitted-argument condition inside parentheses (2023 8.8.4.8):
*> "( a OMITTED ) OR ( b IS NOT OMITTED )" opens a condition, not an
*> arithmetic expression -- X-COBOL's cobcurses writes it so, and the
*> parenthesis was taken for an expression.  A class-name in parentheses
*> likewise.
identification division.
program-id. omitparen.
environment division.
configuration section.
special-names.
    class hexdig is "0" thru "9" "a" thru "f".
data division.
working-storage section.
01 x pic x(4) value "beef".
01 y pic x(4) value "abcd".
procedure division.
    call "both" using x y
    call "both" using omitted y
    call "both" using x omitted
    call "both" using omitted omitted
    if ( x hexdig ) and ( y is not hexdig )
        display "class in parentheses wrong"
    else
        display "class in parentheses"
    end-if
    stop run.
end program omitparen.

identification division.
program-id. both.
data division.
linkage section.
01 a pic x(4).
01 b pic x(4).
procedure division using optional a optional b.
    if ( a omitted ) or ( b is not omitted )
        display "a omitted or b passed"
    else
        display "a passed, b omitted"
    end-if
    if not ( ( a omitted ) and ( b omitted ) )
        display "  one passed"
    else
        display "  none passed"
    end-if
    goback.
end program both.
