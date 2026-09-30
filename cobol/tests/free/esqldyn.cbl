*> EXEC SQL, dynamic (docs/esql.md, phase 3): EXECUTE IMMEDIATE of a host
*> variable and a literal, PREPARE / EXECUTE USING and INTO, a cursor FOR a
*> prepared statement, GET DIAGNOSTICS (statement and condition items),
*> SET TRANSACTION, DEALLOCATE PREPARE; then the SQL descriptors: ALLOCATE,
*> SET DESCRIPTOR (COUNT, TYPE, LENGTH, PRECISION, SCALE, INDICATOR, DATA),
*> EXECUTE USING SQL DESCRIPTOR with a NULL, DESCRIBE OUTPUT, OPEN USING
*> and FETCH INTO SQL DESCRIPTOR, GET DESCRIPTOR, DEALLOCATE (33000 after).
*> No oracle: GnuCOBOL has no precompiler of its own.
identification division.
program-id. dyn.
data division.
working-storage section.
01 stmt     pic x(200).
01 k        pic x(3).
01 v        pic s9(5) comp.
01 n        pic s9(9) comp.
01 cmd      pic x(30).
01 dcmd     pic x(30).
01 st5      pic x(5).
01 msg      pic x(60).
01 cnt      pic s9(4) comp.
01 rows     pic s9(9) comp.
01 sqlcode  pic s9(9) comp.
01 sqlstate pic x(5).
procedure division.
    move "create table d (k char(3), v integer)" to stmt
    exec sql execute immediate :stmt end-exec
    display "immediate " sqlcode
    exec sql get diagnostics :cmd = command_function, :dcmd = dynamic_function, :cnt = number end-exec
    display "diag [" cmd "] [" dcmd "] " cnt
    exec sql execute immediate 'insert into d values (''A'', 1)' end-exec
    exec sql prepare ins from 'insert into d values (?, ?)' end-exec
    display "prepare " sqlcode
    move "B" to k move 2 to v
    exec sql execute ins using :k, :v end-exec
    move "C" to k move 3 to v
    exec sql execute ins using :k, :v end-exec
    display "execute " sqlcode
    exec sql get diagnostics :rows = row_count end-exec
    display "rows " rows
    move "select sum(v) from d where k > ?" to stmt
    exec sql prepare sel from :stmt end-exec
    move "A" to k
    exec sql execute sel into :n using :k end-exec
    display "sum " n
    exec sql prepare q from 'select k, v from d order by v desc' end-exec
    exec sql declare cq cursor for q end-exec
    exec sql open cq end-exec
    perform until sqlcode not = 0
        exec sql fetch cq into :k, :v end-exec
        if sqlcode = 0 display "row " k " " v end-if
    end-perform
    exec sql close cq end-exec
    exec sql execute immediate 'insert into nosuch values (1)' end-exec
    display "bad " sqlcode " " sqlstate
    exec sql get diagnostics exception 1 :st5 = returned_sqlstate, :msg = message_text end-exec
    display "diag state " st5 " msg [" msg "] now " sqlcode
    exec sql execute nevprep end-exec
    display "unprepared " sqlcode " " sqlstate
    exec sql set transaction read only end-exec
    display "set trans " sqlcode
    exec sql deallocate prepare ins end-exec
    display "dealloc " sqlcode
    exec sql commit work end-exec
    stop run.
