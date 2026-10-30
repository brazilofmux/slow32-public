* Commands: program and console statements -- DO CASE, FOR STEP, EXIT and
* LOOP, declarations, STORE, SAVE / RESTORE ADDITIVE, TEXT, comments
LOCAL i, j := 0, k   && a trailing comment
PRIVATE p1 := 1, p2 := "two"
PUBLIC pub1
DECLARE arr[3]
? VALTYPE(p1), p2, VALTYPE(pub1), LEN(arr), pub1
FOR i := 10 TO 1 STEP -3
   ?? i
NEXT
?
FOR i := 1 TO 10
   IF i == 2
      LOOP
   ENDIF
   IF i > 4
      EXIT
   ENDIF
   ?? i
NEXT
?
i := 0
DO WHILE .T.
   i++
   IF i >= 3
      EXIT
   ENDIF
ENDDO
? "while", i
DO CASE
CASE i == 1
   ? "one"
CASE i == 3
   ? "three"
OTHERWISE
   ? "other"
ENDCASE
DO CASE
CASE .F.
   ? "no"
OTHERWISE
   ? "otherwise"
ENDCASE
STORE 7 TO i, j, k
? i + j + k
STORE "x" TO p1
SAVE TO cpc_mem ALL LIKE p*
p1 := "changed"
p2 := "changed"
RESTORE FROM cpc_mem ADDITIVE
? p1, p2, i
TEXT
  literal text, kept as is
ENDTEXT
* a star comment
NOTE a note comment
? "after text"   // a slash comment
EXTERNAL Str
REQUEST Upper
? "declared externals"
FERASE("cpc_mem.mem")
