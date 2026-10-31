// Arithmetic on the empty date: below the first calendar day a date is
// 00/00/00 (DTOS 00000000), at or below zero it is blank; it was decoded
// into characters that are not digits ("11/26//-").
PROCEDURE Main()
   LOCAL d := CTOD("")
   ? DOW(d), EMPTY(d)
   ? "[" + DTOC(d + 2) + "]", EMPTY(d + 2), d + 2 - d
   ? "[" + DTOC(d - DOW(d) + 2) + "]"
   ? "[" + DTOS(d + 2) + "]"
   ? "[" + DTOC(d - 1) + "]", EMPTY(d - 1)
RETURN
