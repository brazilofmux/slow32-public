*> SET condition-name TO TRUE (X3.23-1985 6.23 general rule 6; 2023
*> 14.9.39.4 rule 6): the first literal of the VALUE clause goes into
*> the conditional variable by the VALUE clause's rules -- so the
*> condition is true afterwards.  For an edited item given an
*> alphanumeric literal, VALUE places the characters as written (2023
*> 13.18.63.3 rules 4 and 7-8); a MOVE would edit them, " 12.5" becoming
*> "   12.50", and the condition would then be false.  That was this
*> compiler's answer until the SET sweep, and it is still GnuCOBOL's
*> (.oracle-expected; docs/oracles.md).  Numeric, signed, packed, ALL
*> and figurative literals, and several condition-names at once.
*> ze-a is false after its SET, in both compilers: a condition-name is
*> tested by the relation rules, and those compare an edited item as its
*> characters, "  12" against the literal's "12" (2023 8.8.4.2.5).
identification division.
program-id. setcond.
data division.
working-storage section.
01 ne   pic z,zz9.99.
   88 ne-a value "1,234.50".
   88 ne-b value " 12.5".
01 ae   pic xxbxx.
   88 ae-a value "abcd".
01 ze   pic zzz9.
   88 ze-a value 12.
01 sn   pic s99 sign leading separate.
   88 sn-m value -5 thru 5.
01 pk   pic s9(5) packed-decimal.
   88 pk-a value 123.
01 gv.
   05 gv1 pic x(2).
   05 gv2 pic 9(2).
   88 gv2-a value 7.
01 gx   pic x(6).
   88 gx-all value all "ab".
   88 gx-sp  value spaces.
   88 gx-z   value zeros.
   88 gx-hv  value high-values.
01 ga pic x(4).
   88 ga-q value quote.
procedure division.
    set ne-a to true display "[" ne "]"
    set ne-b ae-a to true display "[" ne "] [" ae "]"
    if ne-b display "ne-b true" else display "ne-b false" end-if
    if ae-a display "ae-a true" else display "ae-a false" end-if
    set ze-a to true display "[" ze "]"
    if ze-a display "ze-a true" else display "ze-a false" end-if
    set sn-m to true display "[" sn "]"
    set pk-a to true display "[" pk "]"
    set gv2-a to true display "[" gv2 "]"
    set gx-all to true display "[" gx "]"
    set gx-z to true display "[" gx "]"
    set gx-sp to true display "[" gx "]"
    set ga-q to true display "[" ga "]"
    set gx-hv to true if gx = high-values display "hv ok" end-if
    stop run.
