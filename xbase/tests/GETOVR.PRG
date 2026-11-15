* A numeric GET in the interpreter overtypes from the left, as dBASE III
* does: "1" typed over 0 in a two-column field is 10
n = 0
@ 1,1 GET n PICTURE "##"
READ
? n
QUIT
