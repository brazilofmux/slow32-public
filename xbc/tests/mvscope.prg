* A variable a procedure makes without declaring it is the procedure's:
* gone when it returns, as dBASE III and Clipper release it.  One it
* finds already made is the caller's, and keeps what it is given; a
* PUBLIC outlives it; a PRIVATE's earlier value comes back; a parameter
* goes.  (Both engines kept every such variable once.)
x = 1
DO MAKES
? "made:", TYPE("w"), "public:", TYPE("pub"), pub
? "caller's x:", x
y = "outer"
DO HIDES
? "y again:", y
DO TAKES WITH 5
? "parameter:", TYPE("n")
RETURN

PROCEDURE MAKES
w = 3
x = 2
PUBLIC pub
pub = "kept"
? "inside:", TYPE("w"), w
RETURN

PROCEDURE HIDES
PRIVATE y
y = 42
? "hidden y:", y
RETURN

PROCEDURE TAKES
PARAMETERS n
? "n:", n
RETURN
