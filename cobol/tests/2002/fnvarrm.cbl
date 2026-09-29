identification division.
program-id. fnvarrm.
*> Reference modification of function results (2023 8.4.3.3.3 rule 2;
*> cobol ISSUES-88): positions are characters, two bytes each in a
*> national result; a result whose length is known only at run time
*> gives a fixed part when the length is written, and a part to its end
*> otherwise, whose LENGTH is counted at run time.  A fixed part that
*> runs past the result as it came out is EC-BOUND-REF-MOD (fatal): the
*> national form of "日本    " has six characters, not its ten-byte
*> maximum.  Computed positions (cobol ISSUES-91) find the part at run
*> time.
*> No oracle (docs/national.md).
data division.
working-storage section.
01  a        pic x(10) value "日本".
01  k        pic 99 value 2.
01  l        pic 99 value 3.
procedure division.
declaratives.
dc section.
    use after exception condition ec-bound-ref-mod.
d1.
    display "  declarative: " function exception-status(1:16).
end declaratives.
main section.
m1.
    display "national-of(café)(2:2): [" function national-of("café")(2:2) "]"
    display "national-of(abcdef)(3:): [" function national-of("abcdef")(3:) "] "
        function length(function national-of("abcdef")(3:))
    display "display-of(日本語)(4:3): [" function display-of(n"日本語")(4:3) "]"
    display "char-national(12354)(1:1): [" function char-national(12354)(1:1) "]"
    display "boolean-of-integer(10, 8)(5:4): " function boolean-of-integer(10, 8)(5:4)
    display "computed, national-of(café)(k:2): [" function national-of("café")(k:2) "]"
    display "computed, display-of(日本語)(k * 3 - 2:3): [" function display-of(n"日本語")(k * 3 - 2:3) "]"
    display "computed, upper-case(abcdef)(k:): [" function upper-case("abcdef")(k:) "] "
        function length(function upper-case("abcdef")(k:))
    display "computed length, national-of(abcdef)(2:l): [" function national-of("abcdef")(2:l) "]"
>>TURN EC-BOUND-REF-MOD CHECKING ON
    display "national-of(a)(2:3): [" function national-of(a)(2:3) "]"
    display "national-of(a)(5:4), past its six characters (fatal):"
    display function national-of(a)(5:4)
    display "not reached"
    stop run.
