* Functions: which errors can be retried -- the file errors, each raised
* in a directory that does not exist, with the Error object's fields
LOCAL bOld := ERRORBLOCK({|e| BREAK(e)})
DBCREATE("probe_rt", {{"N", "N", 3, 0}})
USE probe_rt
Try("use",    {|| DBUSEAREA(.T., , "nodir/none")})
Try("create", {|| DBCREATE("nodir/none", {{"N", "N", 3, 0}})})
DBSELECTAREA("probe_rt")
Try("index",  {|| DBCREATEINDEX("nodir/none", "N", {|| N})})
Try("copy",   {|| CopyTo()})
Try("alt",    {|| AltTo()})
Try("divide", {|| 1 / 0})
Try("type",   {|| "a" + 1})
Try("undef",  {|| UndefVar + 1})
* a handler that makes the variable and asks for the operation again
ERRORBLOCK({|e| MakeIt(e)})
? "retried", RetryVar + 1
ERRORBLOCK(bOld)
DBCLOSEALL()
FERASE("probe_rt.dbf")

FUNCTION MakeIt(e)
   IF e:genCode == 14 .AND. e:canRetry
      PUBLIC RetryVar
      RetryVar := 42
      RETURN .T.
   ENDIF
   RETURN .F.

FUNCTION CopyTo()
   COPY TO nodir/none
   RETURN NIL

FUNCTION AltTo()
   SET ALTERNATE TO nodir/none
   RETURN NIL

FUNCTION Try(cWhat, b)
   LOCAL e
   BEGIN SEQUENCE
      EVAL(b)
      ? cWhat, "no error"
   RECOVER USING e
      ? cWhat, e:genCode, e:subSystem, e:subCode, e:description, ;
        e:canRetry, e:canDefault, e:canSubstitute
   END SEQUENCE
   RETURN NIL
