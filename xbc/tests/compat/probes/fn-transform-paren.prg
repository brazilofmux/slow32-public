// @( puts a negative number in parentheses: "(" takes the field's first
// place and ")" follows; a number with a digit in that place has no room
// for it and is stars.
PROCEDURE Main
   ? "[" + TRANSFORM(-5, "@( 999") + "]"
   ? "[" + TRANSFORM(-12, "@( 9999") + "]"
   ? "[" + TRANSFORM(-1234, "@( 9999") + "]"
   ? "[" + TRANSFORM(5, "@( 999") + "]"
   ? "[" + TRANSFORM(-3.5, "@( 99.9") + "]"
   ? "[" + TRANSFORM(-1234567, "@(E 9,999,999") + "]"
   ? "[" + TRANSFORM(-42, "@(") + "]"
RETURN
