identification division.
program-id. p20.
*> FLOAT-INFINITY under -std=2002: the 2014 class conditions name the switch
*> that takes them (item 21).
data division.
working-storage section.
01 a usage float-long value 1.
procedure division.
    if a is float-infinity display 'i' end-if
    goback.
