* CA: SYS/ERRORSYS.PRG
* CA's default error handler installed over ours: its own ? and ??
* (OutErr), and the one error it answers without a dialogue -- a zero
* divisor, for which DefError() returns 0 and the expression takes it.
#include "error.ch"
ErrorSys()
? "errorsys installed"
? "zero divide", 5 / 0, 7 / 0 + 1
? "after zero divide", 10 / 2
