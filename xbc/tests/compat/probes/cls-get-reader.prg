// READMODAL over a GetList: each Get read by its own reader block, or the
// standard one, from the keys KEYBOARD puts in the buffer; a reader built
// on GetPreValidate, GetApplyKey and GetPostValidate, as Clipper programs
// extend the Get system.
#include "getexit.ch"
#include "inkey.ch"
PROCEDURE Main
   LOCAL cName := SPACE(6), nQty := 0, cCode := "  "
   LOCAL GetList := {}
   @ 1, 1 GET cName
   @ 2, 1 GET nQty PICTURE "999"
   @ 3, 1 GET cCode
   ATAIL(GetList):reader := {|g| UpperReader(g)}
   KEYBOARD "abc" + CHR(K_ENTER) + "42" + CHR(K_ENTER) + "xy" + CHR(K_ENTER)
   ? READMODAL(GetList)
   ? "[" + cName + "]", nQty, "[" + cCode + "]", UPDATED()
   RETURN

STATIC PROCEDURE UpperReader(oGet)
   LOCAL nKey
   IF GetPreValidate(oGet)
      oGet:setFocus()
      DO WHILE oGet:exitState == GE_NOEXIT
         nKey := INKEY(0)
         IF nKey >= 32 .AND. nKey < 127
            nKey := ASC(UPPER(CHR(nKey)))
         ENDIF
         GetApplyKey(oGet, nKey)
      ENDDO
      IF !GetPostValidate(oGet)
         oGet:exitState := GE_NOEXIT
      ENDIF
      oGet:killFocus()
   ENDIF
   RETURN
