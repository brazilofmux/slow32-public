identification division.
program-id. valfalse.
*> The FALSE phrase (2023 13.18.63.3 rule 27; 14.9.39.3 rule 7): its
*> literal is none of the condition's values, and SET ... TO FALSE needs
*> one.
data division.
working-storage section.
01 a pic 99.
   88 a1 value 1 thru 9 false 5.
   88 a2 value 10.
procedure division.
    set a2 to false
    stop run.
