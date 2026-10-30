* Functions: VALTYPE() for each type
LOCAL a := {1}, b := {|| 1}
? VALTYPE(1), VALTYPE("c"), VALTYPE(.T.), VALTYPE(DATE()), VALTYPE(a), VALTYPE(b), VALTYPE(NIL)
