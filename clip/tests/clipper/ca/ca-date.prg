* CA: SAMPLE/DATE.PRG
LOCAL d := CTOD("03/15/2024"), a
SET CENTURY ON
? Mdy(d), Dmy(d)
? AddMonth(d, 3), AddMonth(d, -2), AddMonth(CTOD("01/31/2024"), 1)
a := DateAsArray(d)
? a[1], a[2], a[3], ArrayAsDate({2024, 2, 29})
? DateIsLeap(d), DateIsLeap(CTOD("01/01/2023")), NtoD(2, 29, 2024)
