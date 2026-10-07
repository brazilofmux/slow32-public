*> START FIRST and LAST of a sequential file (2023 14.9.41, general rules
*> 20-21: by position, no key) and READ PREVIOUS of one (14.9.30: records
*> of fixed length have a place to step back to): after START LAST, READ
*> PREVIOUS reads the last record; after START FIRST, READ NEXT the first;
*> NEXT and PREVIOUS mixed; past the beginning backwards, AT END, then
*> 46 either way, as an indexed file's; FIRST and LAST of an empty file,
*> status 23; LAST of a file of variable-length records (found by walking
*> them; READ PREVIOUS of such a file is refused, bad/std2002-readprev-varying).  No oracle:
*> GnuCOBOL 4 has neither START nor READ PREVIOUS of a sequential file.
*> docs/conformance/io-statements.md
identification division.
program-id. startseq.
environment division.
input-output section.
file-control.
    select sq assign to "tmp/ss.seq" organization sequential access sequential
        file status fs.
    select sv assign to "tmp/sv.seq" organization sequential file status fs.
data division.
file section.
fd sq.
01 sr pic x(5).
fd sv record varying from 3 to 8 depending on ln.
01 vr pic x(8).
working-storage section.
01 fs pic xx.
01 i pic 9.
01 ln pic 9.
procedure division.
    open output sq
    perform varying i from 1 by 1 until i > 4
        move "rec" to sr move i to sr(5:1) write sr
    end-perform
    close sq
    open input sq
    read sq display "read  " sr
    start sq last
    display "last  " fs
    read sq previous display "prev  " sr
    read sq previous display "prev  " sr
    read sq next display "next  " sr
    read sq previous display "prev  " sr
    read sq previous display "prev  " sr
    read sq previous display "prev  " sr
    read sq previous at end display "prev  end " fs not at end display "prev  " sr end-read
    read sq previous display "prev  " fs
    read sq next display "next  " fs
    start sq first
    display "first " fs
    read sq next display "next  " sr
    read sq previous at end display "prev  end " fs not at end display "prev  " sr end-read
    start sq first
    read sq previous display "prev  " sr
    read sq next display "next  " sr
    read sq next display "next  " sr
    read sq next display "next  " sr
    read sq next at end display "next  end " fs not at end display "next  " sr end-read
    read sq previous display "prev  " fs
    close sq
    open output sq close sq
    open input sq
    start sq first
    display "empty " fs
    start sq last
    display "empty " fs
    close sq
    open output sv
    move 3 to ln move "abc" to vr write vr
    move 8 to ln move "defghijk" to vr write vr
    move 5 to ln move "lmnop" to vr write vr
    close sv
    open input sv
    start sv last
    display "vlast " fs
    read sv next display "vnext " fs " " ln " " vr
    read sv next at end display "vnext end " fs end-read
    start sv first
    read sv next display "vnext " fs " " ln " " vr
    close sv
    stop run.
