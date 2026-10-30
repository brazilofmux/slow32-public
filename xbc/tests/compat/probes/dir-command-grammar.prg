* Directives: #command's grammar -- optional clauses in any order,
* restricted markers and logify, list markers, smart stringify, blockify,
* wild markers, #translate within a line, four-letter keywords
#command SHOWIT <x> [WIDTH <w>] [UPPER <u:YES,NO>] [TIMES <n>] => ShowArgs(<x>, <w>, <(u)>, <n>, <.u.>)
#command LISTEM <a,...> => ? "list", {<a>}, LEN({<a>})
#command NAME <x> => ? "name", <(x)>, <"x">
#command LATER <x> => ? "later", EVAL(<{x}>)
#command ECHO <*rest*> => ? "echo", #<rest>
#xcommand STRICTWORD <x> => ? "strict", <x>
#translate SQUARE(<v>) => ((<v>) * (<v>))
LOCAL k := 3
SHOWIT "a"
SHOWIT "b" WIDTH 10
SHOWIT "c" TIMES 2 WIDTH 5
SHOWIT "d" UPPER YES TIMES 1
SHOWIT "e" UPPER NO
LISTEM 1, 2 + 3, "x"
NAME (k + 1)
NAME "quoted"
NAME k
LATER k * 7
ECHO anything at all, really
SHOW "abbreviated"
? SQUARE(k) + SQUARE(2), "and", SQUARE(k + 1)
RETURN

FUNCTION ShowArgs(x, w, uw, n, u)
? "show", x, w, uw, n, u
RETURN NIL
