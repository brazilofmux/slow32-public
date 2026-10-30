* Functions: the date functions on an empty date -- YEAR(), MONTH(),
* DAY(), DOW(), CDOW(), CMONTH(), DTOS(), DTOC(), EMPTY() and arithmetic
LOCAL d := CTOD("")
? YEAR(d), MONTH(d), DAY(d), DOW(d)
? "[" + CDOW(d) + "]", "[" + CMONTH(d) + "]", "[" + DTOS(d) + "]", "[" + DTOC(d) + "]"
? EMPTY(d), VALTYPE(d), d == CTOD("  /  /  ")
? EMPTY(CTOD("13/45/92")), DTOC(CTOD("13/45/92"))
