>>COBOL-WORDS EQUATE "DISPLAY" WITH "SHOW"
>>COBOL-WORDS UNDEFINE "PAGE"
>>COBOL-WORDS SUBSTITUTE "PERFORM" BY "DO"
>>COBOL-WORDS RESERVE "MYWORD"
>>DEFINE K AS 3
>>DISPLAY "k is " K " and " K * 2 + 1 UPON LISTING
>>DISPLAY PARAMETER NOSUCH
>>PUSH DEFINE
>>DEFINE K AS 7 OVERRIDE
>>DISPLAY "k now " K
>>POP DEFINE
>>DISPLAY "k back " K
>>POP DEFINE
*> The 2023 directives (docs/plans/standard-queue.md item 34): >>COBOL-WORDS
*> (7.3.10: a synonym, a freed word, a substitute, a reserved one),
*> >>DISPLAY (7.3.12, to the standard error at compile time), >>PUSH and
*> >>POP (7.3.22, 7.3.20: DEFINE at the text stage, TURN and ALL at the
*> parser's; a POP with nothing pushed is a warning). No oracle: GnuCOBOL 4
*> has none of these.
identification division.
program-id. directives.
data division.
working-storage section.
01 page pic 9(3) value 42.
01 perform pic x(5) value "hello".
01 n pic 9 value 1.
01 k pic 99 value 9.
01 ne pic zz9.
procedure division.
    show "page=" page " perform=" perform.
    do until n > 2
        show "n=" n
        add 1 to n
    end-perform.
>>IF K = 3
    show "K is three".
>>ELSE
    show "K is not three".
>>END-IF
    >>TURN EC-CONTINUE-LESS-THAN-ZERO CHECKING ON
    >>PUSH TURN
    >>TURN EC-CONTINUE-LESS-THAN-ZERO CHECKING OFF
    continue after -1 seconds.
    show "off: [" function exception-status "]".
    >>POP TURN
    continue after -1 seconds.
    show "on: [" function exception-status "]".
    >>PUSH ALL
    >>REF-MOD-ZERO-LENGTH ON
    move perform(2:0) to perform.
    show "zero ok".
    >>POP ALL
    >>POP TURN
>>PUSH SOURCE
>>SOURCE FORMAT FIXED
      * a fixed-form comment line
           show "fixed form".
      >>POP SOURCE
    show "free again".
    stop run.
