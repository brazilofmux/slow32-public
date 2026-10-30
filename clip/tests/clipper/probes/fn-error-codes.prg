* Functions: the Error object's genCode, description, operation and
* subCode for the common errors, as error.ch names them
#include "error.ch"
LOCAL bOld := ERRORBLOCK({|e| BREAK(e)}), e, a := {1, 2}, x
BEGIN SEQUENCE
   x := 1 + "a"
RECOVER USING e
   ? e:genCode == EG_ARG, e:description, e:operation, e:subCode, e:severity
END
BEGIN SEQUENCE
   x := 1 / 0
RECOVER USING e
   ? e:genCode == EG_ZERODIV, e:description, e:operation, e:subCode
END
BEGIN SEQUENCE
   x := a[5]
RECOVER USING e
   ? e:genCode == EG_BOUND, e:description, e:operation, e:subCode
END
BEGIN SEQUENCE
   x := nowhere
RECOVER USING e
   ? e:genCode == EG_NOVAR, e:description, e:operation, e:subCode
END
BEGIN SEQUENCE
   x := "a" - 1
RECOVER USING e
   ? e:genCode == EG_ARG, e:description, e:operation, e:subCode
END
ERRORBLOCK(bOld)
