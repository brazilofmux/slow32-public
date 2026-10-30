* A GET on a field stores into the record and nothing else: it once also
* made a memvar of the field's name, which the next GET on that field
* found first (DBU's second new field showed the first's name and kept
* none of its own).  And CREATE FROM through a macro keeps its clauses.
SELECT 10
CREATE strux.ext
APPEND BLANK
REPLACE field_type WITH "C", field_len WITH 20
@ 1,1 GET field_name
READ
APPEND BLANK
REPLACE field_type WITH "N", field_len WITH 3
@ 2,1 GET field_name
READ
? "record 2:", "[" + field_name + "]", TYPE("field_name")
GO 1
? "record 1:", "[" + field_name + "]"
USE
filename = "NEWF.DBF"
cAlias = "whatever"
CREATE &filename FROM strux.ext ALIAS cAlias
? ALIAS(), FCOUNT(), FIELDNAME(1), FIELDNAME(2)
USE
ERASE strux.ext
ERASE NEWF.DBF
RETURN
