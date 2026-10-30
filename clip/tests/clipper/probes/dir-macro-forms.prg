// Macro forms as DBU writes them: &name beside M-> in one expression,
// &name[i] with an M-> index, a statement that begins with a macro,
// &(expr) compiled at run time (a block among them), and names computed
// in parentheses on SELECT and USE.
PROCEDURE Main
   LOCAL cName := "aList", bAlias, cDb, cAl
   PRIVATE page := 2, el := "_el3", _el3 := {10, 20, 30}, aList := {"A->X", "Y"}
   PRIVATE cur_area := 3

   ? "&el[M->page]", &el[M->page]
   &el[M->page] := 99
   ? "stored", _el3[2]
   &el[M->page + 1] := &el[M->page] + 1
   ? "stored 2", _el3[3]

   ? "&(sum)", &("1 + 2")
   bAlias := &("{|i| iif('->' $ " + cName + "[i], Substr(" + cName + "[i], 1, At('->', " + cName + "[i]) - 1), '')}")
   ? "&(block)", EVAL(bAlias, 1), "[" + EVAL(bAlias, 2) + "]"
   ? "&(nested)", &("M->page") + &("&('page') * 2")

   SELECT (M->cur_area)
   ? "select", SELECT()
   SELECT (1)
   ? "select 1", SELECT()
   M->cur_area := 2
   ? "M-> store", cur_area
   RETURN
