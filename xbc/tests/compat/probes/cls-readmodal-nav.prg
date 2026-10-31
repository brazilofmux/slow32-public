// READMODAL's movement: Up and Down, a WHEN that skips a Get the way the
// read is going, a VALID that refuses until it is satisfied, Esc's undo,
// typing past a field's end, READEXIT() at the top, and Ins.
#include "inkey.ch"
PROCEDURE Main
   LOCAL a := "aa", b := "bb", c := 0, d := "dd", lSkip := .T.
   LOCAL GetList := {}
   @ 1, 1 GET a
   @ 2, 1 GET b WHEN !lSkip
   @ 3, 1 GET c PICTURE "99" VALID c > 10
   @ 4, 1 GET d
   // a: "xy" fills it and moves on; b is skipped; c: 5 refused, then 42;
   // Up from c skips b again to a; Down, Down to d; Esc ends the READ
   KEYBOARD "xy" + "5" + CHR(K_ENTER) + CHR(K_HOME) + "42" + CHR(K_UP) + ;
            CHR(K_DOWN) + CHR(K_DOWN) + "zz" + CHR(K_ESC)
   ? READMODAL(GetList), "[" + a + "]", "[" + b + "]", c, "[" + d + "]", UPDATED()
   // Esc undoes the Get it is on
   GetList := {}
   a := "aa"
   @ 1, 1 GET a
   KEYBOARD "q" + CHR(K_ESC)
   ? READMODAL(GetList), "[" + a + "]", LASTKEY() == K_ESC
   // READEXIT(.T.): Up from the first Get ends the READ
   GetList := {}
   @ 1, 1 GET a
   @ 2, 1 GET d
   READEXIT(.T.)
   KEYBOARD "m" + CHR(K_UP)
   ? READMODAL(GetList), "[" + a + "]"
   READEXIT(.F.)
   // Ins: insert mode pushes the rest of the field along
   GetList := {}
   a := "ab"
   d := "cd"
   @ 1, 1 GET a
   @ 2, 1 GET d
   KEYBOARD CHR(K_INS) + "z" + CHR(K_ENTER) + CHR(K_ENTER)
   READMODAL(GetList)
   ? "[" + a + "]", READINSERT()
   READINSERT(.F.)
   RETURN
