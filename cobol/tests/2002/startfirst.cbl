*> START FIRST and LAST (2023 14.9.41, general rules 11-12, 18-19) on an
*> indexed file -- by the prime key, which becomes the key of reference
*> whatever the last START used -- and on a relative file with holes;
*> each followed by READ NEXT and READ PREVIOUS, to the end both ways
*> (AT END, then 46); FIRST and LAST on an empty file (status 23); and
*> WITH LENGTH (general rules 13-14): the leading characters of the key
*> compared, an expression, and a length outside 1 to the key's length
*> (status 23).  Sequential files: startseq.
*> docs/conformance/io-statements.md
identification division.
program-id. startfirst.
environment division.
input-output section.
file-control.
    select ix assign to "tmp/sf.idx" organization indexed access dynamic
        record key ik alternate record key ia with duplicates
        file status fs.
    select rl assign to "tmp/sf.rel" organization relative access dynamic
        relative key rk file status fs.
data division.
file section.
fd ix.
01 ir.
   05 ik pic x(4).
   05 ia pic x.
fd rl.
01 rr pic x(6).
working-storage section.
01 fs pic xx.
01 rk pic 99.
01 n pic 9 value 2.
procedure division.
    open output ix
    move "30ab" to ir write ir
    move "10ac" to ir write ir
    move "20ba" to ir write ir
    move "40bb" to ir write ir
    move "31aa" to ir write ir
    close ix
    open input ix
    start ix first
    display "first " fs
    read ix next display "next  " ir
    read ix previous at end display "prev  end " fs not at end display "prev  " ir end-read
    read ix previous display "prev  " fs
    start ix last
    display "last  " fs
    read ix previous display "prev  " ir
    read ix next at end display "next  end " fs not at end display "next  " ir end-read
    read ix next display "next  " fs
*>  LAST by the prime key, though the key of reference was the alternate
    move "b" to ia
    start ix key = ia
    read ix next display "alt   " ir
    start ix last
    read ix previous display "prime " ir
*>  WITH LENGTH: the leading n characters of the key
    move "3xxx" to ik
    start ix key >= ik with length 1
    display "len1  " fs
    read ix next display "len1  " ir
    read ix next display "len1  " ir
    move "31zz" to ik
    start ix key = ik with length n
    display "len2  " fs
    read ix next display "len2  " ir
    move "31zz" to ik
    start ix key = ik with length n + 1
    display "len3  " fs
    start ix key = ik with length 0
    display "len0  " fs
    start ix key = ik with length 5
    display "len5  " fs
    close ix
    open output ix close ix
    open input ix
    start ix first
    display "empty " fs
    start ix last
    display "empty " fs
    close ix

    open output rl
    move 3 to rk move "three " to rr write rr
    move 7 to rk move "seven " to rr write rr
    move 5 to rk move "five  " to rr write rr
    close rl
    open input rl
    start rl first
    display "first " fs
    read rl next display "next  " rk " " rr
    read rl next display "next  " rk " " rr
    read rl previous display "prev  " rk " " rr
    read rl previous at end display "prev  end " fs not at end display "prev  " rk " " rr end-read
    read rl next display "next  " fs
    start rl last
    display "last  " fs
    read rl previous display "prev  " rk " " rr
    read rl previous display "prev  " rk " " rr
    read rl previous display "prev  " rk " " rr
    read rl previous at end display "prev  end " fs not at end display "prev  " rk " " rr end-read
    start rl last
    read rl next display "next  " rk " " rr
    read rl next at end display "next  end " fs not at end display "next  " rk " " rr end-read
    read rl previous display "prev  " fs
    close rl
    open output rl close rl
    open input rl
    start rl first
    display "empty " fs
    start rl last
    display "empty " fs
    close rl
    stop run.
