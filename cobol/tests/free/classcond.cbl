identification division.
program-id. classcond.
*> SPECIAL-NAMES CLASS: user-defined class conditions (Nucleus level 2).
*> A class is a set of characters given as one-character literals and
*> THROUGH ranges; "x IS class" holds when every character of x is in it.
environment division.
configuration section.
special-names.
    class digits is '0' through '9'
    class vowels is 'a' 'e' 'i' 'o' 'u' 'A' 'E' 'I' 'O' 'U'
    class hexdig is '0' thru '9' 'a' thru 'f' 'A' thru 'F'.
data division.
working-storage section.
01  ws-code          pic x(12).
01  ws-i             pic 99 comp.
01  ws-n             pic 99.
01  ws-word          pic x(6).
procedure division.
main.
    move '4791 2' to ws-code.
    if ws-code is digits display 'all digits' else display 'not all digits'.
    move '479132' to ws-code(1:6).
    if ws-code(1:6) is digits display 'first six digits'.
    if ws-code(1:6) not digits display 'oops'.
    if ws-code(7:6) is digits display 'oops' else display 'spaces are not digits'.
*>  the damm loop: count leading digits
    move 0 to ws-n.
    move 'x' to ws-code(4:1).
    perform varying ws-i from 1 by 1
        until ws-i > length of ws-code or ws-code(ws-i:1) not digits
        add 1 to ws-n
    end-perform.
    display 'leading digits: ' ws-n.
    move 'aeiOU ' to ws-word.
    if ws-word(1:5) is vowels display 'vowels' end-if.
    if ws-word is vowels display 'oops' else display 'space is not a vowel' end-if.
    move 'beef' to ws-word.
    if ws-word(1:4) is hexdig display 'hex'.
    if ws-word(1:4) is vowels display 'oops' else display 'not vowels'.
    move 'DEADg' to ws-word.
    if ws-word(1:5) is hexdig display 'oops' else display 'g is not hex'.
    if ws-word(1:4) is hexdig and ws-word(1:4) is alphabetic-upper display 'hex and upper'.
    stop run.
