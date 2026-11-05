REM a DIM under '$DYNAMIC may be REDIMmed, and DIMmed again where it would fail only if it ran
'$DYNAMIC
DIM a%(3)
REDIM a%(5)
DIM b%(3)
DIM b%(3)
'$STATIC
REDIM a%(7)
