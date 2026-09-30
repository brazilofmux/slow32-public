*> EXEC SQL, dynamic (docs/esql.md, phase 3): EXECUTE IMMEDIATE of a host
*> variable and a literal, PREPARE / EXECUTE USING and INTO, a cursor FOR a
*> prepared statement, GET DIAGNOSTICS (statement and condition items),
*> SET TRANSACTION, DEALLOCATE PREPARE; then the SQL descriptors: ALLOCATE,
*> SET DESCRIPTOR (COUNT, TYPE, LENGTH, PRECISION, SCALE, INDICATOR, DATA),
*> EXECUTE USING SQL DESCRIPTOR with a NULL, DESCRIBE OUTPUT, OPEN USING
*> and FETCH INTO SQL DESCRIPTOR, GET DESCRIPTOR, DEALLOCATE (33000 after).
*> No oracle: GnuCOBOL has no precompiler of its own.
identification division.
program-id. dsc.
data division.
working-storage section.
01 k        pic x(3).
01 v        pic s9(5) comp.
01 amt      pic s9(5)v99.
01 cnt      pic s9(4) comp.
01 typ      pic s9(4) comp.
01 len      pic s9(4) comp.
01 prec     pic s9(4) comp.
01 scl      pic s9(4) comp.
01 ind      pic s9(4) comp.
01 nm       pic x(20).
01 dname    pic x(10) value "OUTD".
01 sqlcode  pic s9(9) comp.
01 sqlstate pic x(5).
procedure division.
    exec sql create table t (k char(3), v integer, amt decimal(7,2)) end-exec
    exec sql allocate descriptor 'IND' with max 5 end-exec
    display "alloc " sqlcode
    exec sql set descriptor 'IND' count = 3 end-exec
    move "AB" to k move 7 to v move 12.50 to amt
    exec sql set descriptor 'IND' value 1 type = 1, length = 3, data = :k end-exec
    exec sql set descriptor 'IND' value 2 type = 4, data = :v end-exec
    exec sql set descriptor 'IND' value 3 type = 3, precision = 7, scale = 2, data = :amt end-exec
    exec sql prepare ins from 'insert into t values (?, ?, ?)' end-exec
    exec sql execute ins using sql descriptor 'IND' end-exec
    display "insert " sqlcode
    exec sql set descriptor 'IND' value 3 type = 3, indicator = -1 end-exec
    move "CD" to k
    exec sql set descriptor 'IND' value 1 type = 1, length = 3, data = :k end-exec
    exec sql execute ins using sql descriptor 'IND' end-exec
    display "insert null " sqlcode
    exec sql prepare q from 'select k, v, amt from t where v > ? order by k' end-exec
    exec sql allocate descriptor :dname end-exec
    exec sql describe output q using sql descriptor :dname end-exec
    exec sql get descriptor :dname :cnt = count end-exec
    display "describe count " cnt
    exec sql get descriptor :dname value 1 :nm = name, :typ = type, :len = length end-exec
    display "col1 " nm " type " typ " len " len
    exec sql get descriptor :dname value 3 :typ = type, :prec = precision, :scl = scale end-exec
    display "col3 type " typ " prec " prec " scale " scl
    exec sql set descriptor 'IND' count = 1 end-exec
    move 0 to v
    exec sql set descriptor 'IND' value 1 type = 4, data = :v end-exec
    exec sql declare c cursor for q end-exec
    exec sql open c using sql descriptor 'IND' end-exec
    perform until sqlcode not = 0
        exec sql fetch c into sql descriptor :dname end-exec
        if sqlcode = 0
            exec sql get descriptor :dname value 1 :k = data end-exec
            exec sql get descriptor :dname value 3 :ind = indicator, :amt = data end-exec
            display "row " k " ind " ind " amt " amt
        end-if
    end-perform
    exec sql close c end-exec
    exec sql deallocate descriptor 'IND' end-exec
    exec sql get descriptor 'IND' :cnt = count end-exec
    display "gone " sqlcode " " sqlstate
    exec sql commit end-exec
    stop run.
