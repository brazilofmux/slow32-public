// ACHOICE() driven by KEYBOARD: the two key sets (with and without a
// user function), selectable arrays, first letters, the modes the user
// function sees and its answers.  No terminal is needed: the typeahead
// buffer is the keyboard.
#include "inkey.ch"
#include "achoice.ch"

PROCEDURE Main
   LOCAL a := {"Apple", "Banana", "Cherry", "Date", "Elder", "Fig", "Grape"}
   LOCAL s := {.T., .T., .F., .T., .T., .T., .T.}

   KEYBOARD CHR(K_DOWN) + CHR(K_DOWN) + CHR(K_ENTER)
   ? "down down enter", ACHOICE(5, 5, 8, 20, a)
   KEYBOARD CHR(K_DOWN) + CHR(K_DOWN) + CHR(K_ENTER)
   ? "over Cherry", ACHOICE(5, 5, 8, 20, a, s)
   KEYBOARD CHR(K_END) + CHR(K_ENTER)
   ? "end", ACHOICE(5, 5, 8, 20, a)
   KEYBOARD CHR(K_END) + CHR(K_UP) + CHR(K_ENTER)
   ? "end up", ACHOICE(5, 5, 8, 20, a)
   KEYBOARD CHR(K_HOME) + CHR(K_ENTER)
   ? "home", ACHOICE(5, 5, 8, 20, a, .T., , 6)
   KEYBOARD "d" + CHR(K_ENTER)
   ? "letter d", ACHOICE(5, 5, 8, 20, a)
   KEYBOARD "G" + "g" + CHR(K_ENTER)
   ? "letter g twice", ACHOICE(5, 5, 8, 20, a)
   KEYBOARD CHR(K_ESC)
   ? "esc", ACHOICE(5, 5, 8, 20, a)
   KEYBOARD CHR(K_LEFT)
   ? "left", ACHOICE(5, 5, 8, 20, a)
   KEYBOARD CHR(K_RIGHT)
   ? "right", ACHOICE(5, 5, 8, 20, a)
   KEYBOARD CHR(K_PGDN) + CHR(K_ENTER)
   ? "pgdn", ACHOICE(5, 5, 8, 20, a)
   KEYBOARD CHR(K_PGDN) + CHR(K_PGDN) + CHR(K_ENTER)
   ? "pgdn pgdn", ACHOICE(5, 5, 8, 20, a)
   KEYBOARD CHR(K_END) + CHR(K_PGUP) + CHR(K_ENTER)
   ? "end pgup", ACHOICE(5, 5, 8, 20, a)
   KEYBOARD CHR(K_CTRL_PGDN) + CHR(K_ENTER)
   ? "ctrl-pgdn", ACHOICE(5, 5, 8, 20, a)
   KEYBOARD CHR(K_CTRL_PGDN) + CHR(K_CTRL_PGUP) + CHR(K_ENTER)
   ? "ctrl-pgup", ACHOICE(5, 5, 8, 20, a)
   KEYBOARD CHR(K_CTRL_END) + CHR(K_ENTER)
   ? "ctrl-end", ACHOICE(5, 5, 8, 20, a)
   KEYBOARD CHR(K_END) + CHR(K_CTRL_HOME) + CHR(K_ENTER)
   ? "end ctrl-home", ACHOICE(5, 5, 8, 20, a)
   KEYBOARD CHR(K_ENTER)
   ? "initial 5", ACHOICE(5, 5, 8, 20, a, .T., , 5)
   // (an unselectable initial item: the guide says the first selectable
   // one is highlighted instead; Harbour keeps the unselectable one.  The
   // guide's rule is in tests/achoice.prg, not probed here.)
   KEYBOARD CHR(K_ENTER)
   ? "initial 5 window row 1", ACHOICE(5, 5, 8, 20, a, .T., , 5, 1)
   ? "none selectable", ACHOICE(5, 5, 8, 20, a, .F.)
   KEYBOARD CHR(K_DOWN) + CHR(K_ENTER)
   ? "macro selectable", ACHOICE(5, 5, 8, 20, a, {".T.", ".F.", ".T.", "1 == 2", ".T.", ".T.", ".T."})
   KEYBOARD CHR(K_DOWN) + CHR(K_DOWN) + CHR(K_DOWN) + CHR(K_ENTER)
   ? "stops at null", ACHOICE(5, 5, 8, 20, {"One", "Two", "", "Four"})

   // with a user function: side keys and letters are exceptions
   KEYBOARD CHR(K_DOWN) + "x" + CHR(K_ENTER)
   ? "uf select", ACHOICE(5, 5, 8, 20, a, .T., "UFunc")
   KEYBOARD CHR(K_UP) + CHR(K_ESC)
   ? "uf hit top, abort", ACHOICE(5, 5, 8, 20, a, .T., "UFunc")
   KEYBOARD CHR(K_HOME) + CHR(K_END) + CHR(K_LEFT) + "e" + CHR(K_ENTER)
   ? "uf goto letter", ACHOICE(5, 5, 8, 20, a, .T., "UFunc")
   KEYBOARD CHR(K_ENTER)
   ? "uf none selectable", ACHOICE(5, 5, 8, 20, a, .F., "UFunc")
   RETURN

FUNCTION UFunc(nMode, nElem, nRow)
   LOCAL k := LASTKEY()
   ? "  uf", nMode, nElem, nRow, k
   IF nMode == AC_NOITEM
      INKEY(0)
      RETURN AC_ABORT
   ENDIF
   IF nMode == AC_EXCEPT
      IF k == K_ENTER
         RETURN AC_SELECT
      ELSEIF k == K_ESC
         RETURN AC_ABORT
      ELSEIF k == K_LEFT
         RETURN AC_CONT
      ELSEIF k >= 32
         RETURN AC_GOTO
      ENDIF
   ENDIF
   RETURN AC_CONT
