*> EVALUATE's partial expressions (COBOL 2014; 2023 14.9.13.3 rules 5-8): a
*> WHEN object that begins with a relational operator, or a class or sign
*> condition without its identifier, [IS] [NOT] ahead of it -- the
*> selection subject goes to its left and the condition is evaluated, so
*> abbreviated combinations work (WHEN > 3 AND < 5); IS ZERO is the sign
*> condition while WHEN ZERO stays the figurative constant (1985); an
*> expression as the subject; a SPECIAL-NAMES class; partial objects
*> under ALSO; TRUE subjects and THRU ranges as before.  GnuCOBOL agrees.
*> docs/conformance/evaluate.md
identification division.
program-id. partialwhen.
environment division.
configuration section.
special-names.
    class vowel is "AEIOU".
data division.
working-storage section.
01 n pic s9(3) value 42.
01 x pic x(3) value "A12".
01 i pic 99 value 0.
procedure division.
    perform varying i from 1 by 1 until i > 9
        move i to n
        evaluate n
            when < 3 display "small " n
            when not < 8 display "big   " n
            when = 5 display "five  " n
            when > 3 and < 5 display "four  " n
            when is equal to 3 display "three " n
            when other display "other " n
        end-evaluate
    end-perform
    move -5 to n
    evaluate n
        when positive display "pos"
        when is not positive display "not pos"
    end-evaluate
    move 0 to n
    evaluate n
        when zero display "zero (the figurative)"
    end-evaluate
    evaluate n
        when is zero display "IS ZERO (the sign condition)"
    end-evaluate
    evaluate x
        when numeric display "numeric"
        when not alphabetic display "not alphabetic"
    end-evaluate
    move "A" to x
    evaluate x(1:1)
        when vowel display "vowel"
    end-evaluate
    evaluate n + 1
        when > 0 display "n + 1 > 0"
    end-evaluate
    evaluate n also x
        when < 1 also not numeric display "both partial"
    end-evaluate
    evaluate true
        when n < 1 display "condition subject, as before"
    end-evaluate
    evaluate n
        when 0 thru 5 display "range, as before"
    end-evaluate
    stop run.
