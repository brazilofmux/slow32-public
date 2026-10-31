* Statements: a LOCAL is its routine's alone -- a macro (TYPE(), &(),
* a block a macro makes) cannot see it, nor can a routine it calls; it
* is no field, though a field share its name; M->name is a memory
* variable beside it, never the LOCAL
PROCEDURE Main
   LOCAL cName := "lexical", nCount := 3
   PRIVATE cPriv := "dynamic"
   ? "type", TYPE("cName"), TYPE("cPriv"), TYPE("nCount")
   ? "macro", Seen("cPriv")
   ? "called", Called()
   ? "block", EVAL({|| cName })
   ? "macro block", EVAL(&("{|| TYPE('cName') }"))
   ? "private", Hidden()
   ? "kept", cName
   ? "field", Fielded()
   ? "memvar", Memvared()
RETURN

STATIC FUNCTION Seen(c)
RETURN &(c)

STATIC FUNCTION Called()
RETURN TYPE("cName") + " " + TYPE("cPriv")

STATIC FUNCTION Hidden()
   PRIVATE cName := "callee's private"
RETURN Reader()

STATIC FUNCTION Reader()
RETURN cName

STATIC FUNCTION Fielded()
   LOCAL TAG := "the local"
   LOCAL cGot
   DBCREATE("sls_t", {{"TAG", "C", 9, 0}})
   USE sls_t
   APPEND BLANK
   REPLACE TAG WITH "the field"
   cGot := TAG + " / " + FIELD->TAG
   USE
   FERASE("sls_t.dbf")
RETURN cGot

STATIC FUNCTION Memvared()
   LOCAL cBoth := "the local"
   M->cBoth := "the memvar"
RETURN cBoth + " / " + M->cBoth + " / " + TYPE("cBoth")
