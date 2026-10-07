*> The simple and complex conditions, swept for docs/conformance/conditions.md
*> (queue item 19b): the class condition (8.8.4.4) by NUMERIC, ALPHABETIC,
*> -LOWER, -UPPER, BOOLEAN, a SPECIAL-NAMES class and an alphabet-name, of
*> display, packed, binary and floating-point items, a part, and a
*> function's result; NUMERIC of a truncating binary holding more than its
*> PICTURE (false) and of a COMP-5 (its bytes, docs/usage.md); the
*> condition-name condition with ranges and lists (8.8.4.5); the switch-
*> status condition and SET ... TO ON (8.8.4.6); the sign condition of an
*> expression and, format 2, of a floating-point item by its sign bit, -0.0
*> NEGATIVE bare and not in parentheses (8.8.4.7); the boolean condition
*> (8.8.4.3); NOT and the precedence NOT, AND, OR with parentheses
*> (8.8.4.10-11).  No oracle: GnuCOBOL 4 has no alphabet-name class
*> condition and tests a float's sign by value.
*> docs/conformance/conditions.md
identification division.
program-id. condsweep.
environment division.
configuration section.
special-names.
    switch-1 is sw1 on status is sw1-on off status is sw1-off
    class hexdig is "0" thru "9" "A" thru "F"
    alphabet alf is "A" thru "Z".
data division.
working-storage section.
01 x pic x(5) value "AB 12".
01 lo pic x(3) value "ab ".
01 n pic 9(3) value 12.
01 nd pic s9(3) value -12.
01 nb pic s9(3).
01 nbw redefines nb pic x(3).
01 c3 pic 9(3) comp-3 value 7.
01 bl pic 1(4) usage bit value b"1010".
01 b1 pic 1 usage bit value b"1".
01 fl usage float-long value -0.0.
01 cb pic 9(2) comp.
01 cw redefines cb pic x(2).
01 c5 pic 9(2) comp-5.
01 c5w redefines c5 pic x(2).
01 t pic x(6) value " 123  ".
01 k pic 99 value 15.
   88 low-k value 0 thru 9.
   88 mid-k value 10 thru 19 25.
   88 odd-k value 1 3 5 7 9 11 13 15 17 19.
01 a pic 9 value 1.
01 b pic 9 value 0.
procedure division.
    if x is alphabetic display "alpha" else display "not alpha" end-if
    if x(1:2) is alphabetic-upper display "upper" end-if
    if lo is alphabetic-lower display "lower" end-if
    if lo is alphabetic display "lower is alphabetic" end-if
    if x(4:2) is numeric display "numeric part" end-if
    if x(4:2) is hexdig display "hexdig" end-if
    if x(1:2) is alf display "alf" end-if
    if x is not alf display "not alf" end-if
    if n is numeric display "n numeric" end-if
    if nd is numeric display "nd numeric" end-if
    move "-1c" to nbw
    if nb is numeric display "x" else display "nb not numeric" end-if
    if c3 is numeric display "c3 numeric" end-if
    if bl is boolean display "bl boolean" end-if
    if x(4:2) is boolean display "x" else display "x not boolean" end-if
    if fl is numeric display "fl numeric" end-if
    move x"FFFF" to cw
    if cb is numeric display "x" else display "comp out of picture" end-if
    move x"FFFF" to c5w
    if c5 is numeric display "comp-5 numeric" end-if
    if function trim(t) is numeric display "trim numeric" end-if
    if function upper-case(lo) is alphabetic-upper display "fn upper" end-if
    if mid-k display "mid" end-if
    if odd-k and not low-k display "odd not low" end-if
    if sw1-off display "sw1 off" end-if
    set sw1 to on
    if sw1-on display "sw1 on" end-if
    if nd is negative display "neg" end-if
    if nd + 20 is positive display "pos" end-if
    if nd * 0 is zero display "zero" end-if
    if fl is negative display "-0.0 negative" end-if
    if fl is zero display "-0.0 zero" end-if
    if (fl) is negative display "x" else display "(-0.0) not negative" end-if
    if (fl) is positive display "x" else display "(-0.0) not positive" end-if
    if b1 display "b1" end-if
    if not b1 display "x" else display "not not-b1" end-if
    if not (a = 1 and b = 1) display "not both" end-if
    if a = 1 or not b = 1 and a = 0 display "prec ok" end-if
    if (a = 1 or not b = 1) and a = 0 display "x" else display "paren ok" end-if
    stop run.
