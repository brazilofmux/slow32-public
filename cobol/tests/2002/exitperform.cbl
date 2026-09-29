identification division.
program-id. exitperform.
*> EXIT PERFORM [CYCLE], EXIT PARAGRAPH, EXIT SECTION (2023 14.9.14
*> formats 3-4; cobol ISSUES-90).  CYCLE ends this pass of the innermost
*> inline PERFORM, plain EXIT PERFORM leaves it; EXIT PARAGRAPH and EXIT
*> SECTION go to the end of the current paragraph or section, before its
*> return, so a PERFORM of it returns.  In an exception-checking PERFORM,
*> EXIT PERFORM goes to FINALLY.  PERFORM UNTIL EXIT loops until an EXIT
*> PERFORM (14.9.28.4 rule 11).
*> No oracle (docs/standards.md).
data division.
working-storage section.
01  i        pic 99.
01  j        pic 99.
01  line-out pic x(40).
01  p        pic 99.
procedure division.
main section.
m1.
    move spaces to line-out  move 1 to p
    perform varying i from 1 by 1 until i > 10
        if i = 3 exit perform cycle end-if
        if i = 6 exit perform end-if
        move i to line-out(p:2)  add 3 to p
    end-perform
    display "varying: " line-out " left at i = " i
    move spaces to line-out  move 1 to p
    perform varying i from 1 by 1 until i > 3
        perform varying j from 1 by 1 until j > 3
            if j = 2 exit perform end-if
            move i(2:1) to line-out(p:1)  move j(2:1) to line-out(p + 1:1)  add 3 to p
        end-perform
    end-perform
    display "nested, the inner left at j = 2: " line-out
    move 0 to i
    perform until exit
        add 1 to i
        if i = 4 exit perform end-if
    end-perform
    display "until exit, left at " i
    perform para-a
    display "back from para-a"
    perform sec-b
    display "back from sec-b"
    perform
        display "ecp: before"
        exit perform
        display "ecp: not reached"
    when exception ec-user-a
        continue
    finally
        display "ecp: finally"
    end-perform
    stop run.
para-a.
    display "para-a: first"
    exit paragraph
    display "para-a: not reached".
sec-b section.
b1.
    display "sec-b: b1"
    exit section.
b2.
    display "sec-b: b2, not reached".
