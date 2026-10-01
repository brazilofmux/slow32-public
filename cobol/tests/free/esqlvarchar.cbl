*> A VARCHAR host variable, DB2's: a group of a level-49 length and a
*> level-49 text, one host variable.  In, the text's first LENGTH bytes go
*> as they are -- trailing spaces included, where a PIC X host variable's
*> are trimmed; out, the value's bytes land in the text and its length in
*> the length, a value too long cut short with 01004.  Added for majesty's
*> pgexport, which must keep every byte (docs/esql.md).  No oracle:
*> GnuCOBOL has no precompiler of its own.
identification division.
program-id. esqlvarchar.
data division.
working-storage section.
    exec sql begin declare section end-exec.
01  v.
    49 v-len        pic s9(4) comp.
    49 v-text       pic x(10).
01  short-v.
    49 short-len    pic s9(4) comp.
    49 short-text   pic x(3).
01  n               pic 9(4).
01  ind             pic s9(4) comp.
01  k               pic 9(4).
    exec sql end declare section end-exec.
01  sqlcode         pic s9(9) comp-5.
01  sqlstate        pic x(5).
procedure division.
    exec sql create table t (k integer, s varchar(20)) end-exec
    move 1 to k  move "ab        " to v-text  move 4 to v-len
    exec sql insert into t values (:k, :v) end-exec
    move 2 to k  move "xyz" to v-text  move 0 to v-len
    exec sql insert into t values (:k, :v) end-exec
    exec sql select length(s) into :n from t where k = 1 end-exec
    display "1 stored length " n " " sqlstate
    move 99 to v-len  move all "#" to v-text
    exec sql select s into :v from t where k = 1 end-exec
    display "2 fetched length " v-len " [" v-text "] " sqlstate
    exec sql select s into :v from t where k = 2 end-exec
    display "3 empty string: length " v-len " [" v-text "] " sqlstate
    exec sql select s into :short-v from t where k = 1 end-exec
    display "4 cut short: length " short-len " [" short-text "] " sqlstate
    exec sql select null into :v :ind from t where k = 1 end-exec
    display "5 null: indicator " ind " " sqlstate
    stop run.
