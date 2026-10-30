* PRIVATEs of the same name at two depths, with an array DECLAREd in the
* inner one: the outer's own value must come back after the inner call,
* and a second outer call must start fresh (DBU's sysmenu after browse).
PUBLIC keystroke
keystroke = 0
outer(1)
outer(2)
RETURN
FUNCTION outer
PARAMETERS n
PRIVATE menu_sel, menu_func
menu_sel = 0
menu_func = n
? "outer", n, "sel", M->menu_sel, menu_sel, "func", M->menu_func
inner()
? "after inner", M->menu_sel, menu_sel, M->menu_func
DO WHILE M->menu_func > 0 .AND. M->menu_sel = 0
   ? "loop entered"
   menu_sel = 1
ENDDO
RETURN NIL
FUNCTION inner
PRIVATE menu_sel
DECLARE arr[3]
menu_sel = 5
arr[1] = M->menu_sel
? "inner", M->menu_sel, arr[1]
RETURN NIL
