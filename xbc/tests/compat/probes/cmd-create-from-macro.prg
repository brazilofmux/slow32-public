* Commands: CREATE <file> FROM <ext> ALIAS <a> with the file a macro -- the
* clauses after the macro are still read (they were lost: a token read
* out of a macro had only the macro's remainder behind it)
LOCAL cFile := "ccfm_new", cAlias := "whatever"
SELECT 10
CREATE ccfm_ext
APPEND BLANK
REPLACE field_name WITH "NAME", field_type WITH "C", field_len WITH 20, field_dec WITH 0
APPEND BLANK
REPLACE field_name WITH "AGE", field_type WITH "N", field_len WITH 3, field_dec WITH 0
USE
CREATE &cFile FROM ccfm_ext ALIAS cAlias
? ALIAS(), FCOUNT(), FIELDNAME(1), FIELDNAME(2), SELECT()
USE
ERASE ccfm_ext.dbf
ERASE ccfm_new.dbf
? FILE("ccfm_new.dbf"), FILE("ccfm_ext.dbf")
