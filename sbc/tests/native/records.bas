' TYPE records in s32-bc: numeric fields of each type (QB's STRING fields
' are fixed-length, which the interpreter does not keep), a SUB's own record,
' SHARED records, re-DIM, SWAP of fields
DECLARE SUB Local1 ()
DECLARE SUB Bump ()
TYPE Item
    Code AS INTEGER
    Qty AS LONG
    Price AS SINGLE
    Total AS DOUBLE
END TYPE
DIM SHARED Cur AS Item
DIM Spare AS Item
Cur.Code = 7.6: Cur.Qty = 100000: Cur.Price = 1 / 3: Cur.Total = Cur.Qty * 2.5#
PRINT Cur.Code; Cur.Qty; Cur.Price; Cur.Total
PRINT Spare.Code; Spare.Price
Spare.Qty = Cur.Qty \ 3
SWAP Cur.Code, Spare.Code: SWAP Cur.Total, Spare.Total
PRINT Cur.Code; Spare.Code; Cur.Total; Spare.Total; Spare.Qty
CALL Local1: CALL Local1
CALL Bump: CALL Bump
PRINT Cur.Qty
FOR I = 1 TO 3
    DIM Tmp AS Item
    Tmp.Qty = Tmp.Qty + I
NEXT
PRINT Tmp.Qty
END

SUB Local1
    DIM Mine AS Item
    Mine.Code = Mine.Code + 1
    PRINT Mine.Code
END SUB

SUB Bump
    Cur.Qty = Cur.Qty + 1
END SUB
