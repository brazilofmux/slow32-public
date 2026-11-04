' DIM x AS type, DIM SHARED, SHARED arrays, in s32-bc
DECLARE SUB Show ()
DECLARE SUB Grow (K%)
DIM SHARED Count AS INTEGER, Label AS STRING, Big
DIM Ratio AS DOUBLE, N AS LONG
DIM SHARED Tally(3)
Count = 2.7: Label = "lbl": Big = 1.5: Ratio = 1 / 3: N = 100000
PRINT Count; Label; Big; Ratio; N * 3
CALL Show: CALL Show
CALL Grow(2): CALL Grow(3)
PRINT Tally(1); Tally(2); Count
END

SUB Show
    DIM Mine AS STRING
    Mine = Mine + "m"
    Count = Count + 1: Label = Label + "!"
    PRINT Count; Label; Big; Mine
END SUB

SUB Grow (K%)
    DIM Here(K%)
    Here(K%) = K% * 10
    Tally(1) = Tally(1) + Here(K%): Tally(2) = UBOUND(Here)
END SUB
