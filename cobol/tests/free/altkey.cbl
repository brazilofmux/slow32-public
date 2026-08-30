identification division.
program-id. altkey.
*> Indexed I-O level 2: ALTERNATE RECORD KEY, with and without
*> DUPLICATES.  A random READ or START names its key, which becomes the
*> key of reference for READ NEXT; duplicates come back in arrival
*> order and announce themselves with status 02; a START on an item
*> that begins where a key begins is a START on that leading part.
environment division.
input-output section.
file-control.
    select emp assign to 'tmp/emp.dat'
        organization is indexed
        access is dynamic
        record key is emp-id
        alternate record key is emp-name
        alternate record key is emp-dept with duplicates
        file status is st.
data division.
file section.
fd  emp.
01  emp-rec.
    05  emp-id      pic 9(4).
    05  emp-name.
        10  emp-last  pic x(6).
        10  emp-first pic x(4).
    05  emp-dept    pic xx.
    05  emp-pay     pic 9(5).
working-storage section.
01  st              pic xx.
01  eof             pic x value 'n'.
01  n               pic 9 value 0.
procedure division.
main.
    open output emp.
    perform add-one 6 times.
    close emp.
    open i-o emp.
    display '--- by name'.
    move 'MARSH ' to emp-last. move spaces to emp-first.
    start emp key is not less than emp-last invalid key display 'start ' st end-start.
    perform read-all.
    display '--- by dept, from D2'.
    move 'D2' to emp-dept.
    start emp key is equal to emp-dept invalid key display 'start ' st end-start.
    perform read-all.
    display '--- random by alternates'.
    move 'D1' to emp-dept. read emp key is emp-dept invalid key display 'r ' st end-read.
    display st ' ' emp-rec.
    read emp next record at end display 'end' not at end display st ' ' emp-rec end-read.
    move 'JONES JIM ' to emp-name. read emp key is emp-name invalid key display 'r ' st end-read.
    display st ' ' emp-rec.
    move 'NOBODY    ' to emp-name. read emp key is emp-name invalid key display 'absent ' st end-read.
    display '--- writes'.
    move 7 to emp-id. move 'JONES JIM ' to emp-name. move 'D3' to emp-dept. move 100 to emp-pay.
    write emp-rec invalid key display 'dup name ' st end-write.
    move 'NEW   ONE ' to emp-name. move 'D1' to emp-dept.
    write emp-rec invalid key display 'w ' st end-write. display 'write dup dept: ' st.
    move 8 to emp-id. move 'OTHER TWO ' to emp-name. move 'D9' to emp-dept.
    write emp-rec invalid key display 'w ' st end-write. display 'write new dept: ' st.
    display '--- rewrite moves a key, delete drops one'.
    move 3 to emp-id. read emp invalid key display 'r ' st end-read.
    move 'D9' to emp-dept. rewrite emp-rec invalid key display 'rw ' st end-rewrite. display 'rewrite: ' st.
    move 2 to emp-id. delete emp invalid key display 'd ' st end-delete.
    move 'D1' to emp-dept.
    start emp key is not less than emp-dept invalid key display 'start ' st end-start.
    perform read-all.
    close emp.
    stop run.
add-one.
    move n to emp-pay.
    evaluate n
        when 0 move 1 to emp-id move 'SMITH ANN ' to emp-name move 'D1' to emp-dept
        when 1 move 2 to emp-id move 'JONES JIM ' to emp-name move 'D2' to emp-dept
        when 2 move 3 to emp-id move 'BROWN BOB ' to emp-name move 'D1' to emp-dept
        when 3 move 4 to emp-id move 'MARSH EVE ' to emp-name move 'D2' to emp-dept
        when 4 move 5 to emp-id move 'YOUNG ZED ' to emp-name move 'D2' to emp-dept
        when 5 move 6 to emp-id move 'ADAMS AMY ' to emp-name move 'D3' to emp-dept
    end-evaluate.
    add 1 to n.
    write emp-rec invalid key display 'write ' st end-write.
read-all.
    move 'n' to eof.
    perform until eof = 'y'
        read emp next record at end move 'y' to eof
            not at end display st ' ' emp-rec
        end-read
    end-perform.
