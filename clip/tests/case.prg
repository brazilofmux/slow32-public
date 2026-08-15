* DO CASE lowered to C if/else if
x = 2
DO CASE
  CASE x = 1
    ? "one"
  CASE x = 2
    ? "two"
  OTHERWISE
    ? "other"
ENDCASE
y = 9
DO CASE
  CASE y < 5
    ? "small"
  OTHERWISE
    ? "big"
ENDCASE
RETURN
