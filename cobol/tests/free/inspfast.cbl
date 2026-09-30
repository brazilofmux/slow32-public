*> INSPECT whose phrases are all one byte, ALL or CHARACTERS, over the
*> whole item: a byte table in one sweep, the first phrase listed taking
*> a byte (libcob cob_inspect_run).  Beside them, the forms that keep
*> the general pass: BEFORE/AFTER, LEADING, FIRST, several bytes.
identification division.
program-id. inspfast.
data division.
working-storage section.
01 s    pic x(30).
01 t1   pic 9(4).
01 t2   pic 9(4).
01 t3   pic 9(4).
procedure division.
    move "banana bandana cabana a-b-c" to s
    move 0 to t1 t2 t3
    inspect s tallying t1 for all "a" t2 for all "n" t3 for characters
    display s " " t1 " " t2 " " t3
    move 0 to t1 t2
    inspect s tallying t1 for characters t2 for all "a"
    display t1 " " t2
    move 0 to t1 t2
    inspect s tallying t1 for all "a" all "b" t2 for all "a"
    display t1 " " t2
    inspect s replacing all "a" by "A" all "n" by "N"
    display s
    inspect s replacing all "b" by "x" characters by "."
    display s
    move "banana bandana cabana a-b-c" to s
    inspect s replacing all "a" by "A" after "d"
    display s
    move 0 to t1
    inspect s tallying t1 for leading "b"
    display t1
    inspect s replacing first "n" by "#"
    display s
    move 0 to t1
    inspect s tallying t1 for all "an"
    display t1
    inspect s converting "abn" to "XYZ"
    display s
    stop run.
