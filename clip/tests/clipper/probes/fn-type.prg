// TYPE(cExp): the type of what the text evaluates to -- fields of the
// current area included (DBU asks TYPE() whether a filter is logical) --
// U for an undefined name, UE for text that does not compile.
PROCEDURE Main
   LOCAL cFilter := "AGE > 40"
   PRIVATE nVar := 1, cVar := "x", dVar := DATE(), lVar := .T., aVar := {1}, bVar := {|| 1}

   DBCREATE("tyfile", {{"NAME", "C", 10, 0}, {"AGE", "N", 3, 0}, {"WHEN", "D", 8, 0}, {"OK", "L", 1, 0}})
   USE tyfile
   APPEND BLANK
   REPLACE NAME WITH "Ada", AGE WITH 36

   ? "fields", TYPE("NAME"), TYPE("AGE"), TYPE("WHEN"), TYPE("OK")
   ? "filter", TYPE(cFilter), TYPE("AGE > 40 .AND. NAME == 'Ada'")
   ? "memvars", TYPE("nVar"), TYPE("cVar"), TYPE("dVar"), TYPE("lVar"), TYPE("aVar"), TYPE("bVar")
   ? "expressions", TYPE("nVar + 1"), TYPE("cVar + 'y'"), TYPE("UPPER(cVar)"), TYPE("DATE() + 1")
   ? "undefined", TYPE("nosuch"), TYPE("nosuch + 1")
   ? "broken", TYPE("1 +"), TYPE("(")
   ? "nil", TYPE("NIL")
   USE
   FERASE("tyfile.dbf")
   RETURN
