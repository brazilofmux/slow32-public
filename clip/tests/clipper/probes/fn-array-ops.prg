* Functions: AADD ASIZE ATAIL AFILL ASCAN ASORT
LOCAL a := {}
AADD(a, 3)
AADD(a, 1)
AADD(a, 2)
? LEN(a), ATAIL(a), ASCAN(a, 1)
ASORT(a)
? a[1], a[2], a[3]
ASIZE(a, 5)
AFILL(a, 9, 4)
? LEN(a), a[4], a[5]
