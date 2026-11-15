* A numeric GET in the interpreter, as dBASE III edits it: "1" typed over
* 0 in a two-column field shows "10" until the GET is left (the old digit
* stays on the screen), and the value is 1
n = 0
@ 1,1 GET n PICTURE "##"
READ
? n
QUIT
