identification division.
program-id. natfiles.
*> National records in files (cobol ISSUES-74).  A line sequential file
*> of national records is UTF-8 text: WRITE encodes, trailing national
*> spaces dropped (2023 14.9.51 rule 21); READ decodes and pads with
*> national spaces (14.9.30 rule 15).  Bytes that are not UTF-8 read as
*> U+FFFD with status 09 (rule 16); a line longer than the record is
*> truncated, 04; a lone surrogate has no UTF-8 form and its WRITE fails
*> with 71 (14.9.51 rule 23).  A record sequential or indexed file holds
*> the record's bytes, UTF-16BE, and a national key orders by code unit.
*> The alphanumeric views read the same files byte for byte.
*> No oracle (docs/national.md).
environment division.
input-output section.
file-control.
    select nl assign to "tmp/natfiles.txt" organization line sequential
        file status is st.
    select al assign to "tmp/natfiles.txt" organization line sequential
        file status is st.
    select ns assign to "tmp/natfiles.dat" organization sequential.
    select as assign to "tmp/natfiles.dat" organization sequential.
    select ix assign to "tmp/natfiles.idx" organization indexed
        access dynamic record key ix-key file status is st.
data division.
file section.
fd  nl.
01  nl-rec group-usage national.
    05 nl-name   pic n(4).
    05 nl-city   pic n(4).
fd  al.
01  al-rec       pic x(24).
fd  ns.
01  ns-rec       pic n(2).
fd  as.
01  as-rec       pic x(4).
fd  ix.
01  ix-rec group-usage national.
    05 ix-key    pic n(2).
    05 ix-val    pic n(3).
working-storage section.
01  st           pic xx.
01  sx           pic x(4).
01  eof          pic x.
procedure division.
main.
    open output nl
    move n"山田" to nl-name  move n"東京" to nl-city
    write nl-rec
    move n"Ann" to nl-name  move spaces to nl-city
    write nl-rec
    move nx"0041D8000042" to nl-rec
    write nl-rec
    display "lone surrogate: " st
    close nl
    open extend al
    move "ab" to al-rec  move x"FF" to al-rec(3:1)  move "c" to al-rec(4:1)
    write al-rec
    move "123456789" to al-rec
    write al-rec
    close al
    open input al
    move "n" to eof
    perform until eof = "y"
        read al at end move "y" to eof
            not at end
                if al-rec(3:1) = x"FF" display "on disk: [ab] X'FF' [c]"
                else display "on disk: [" al-rec "]" end-if
        end-read
    end-perform
    close al
    open input nl
    move "n" to eof
    perform until eof = "y"
        read nl at end move "y" to eof
            not at end display "national " st ": [" nl-rec "]"
        end-read
    end-perform
    close nl
    open output ns
    move n"日本" to ns-rec  write ns-rec
    close ns
    open input as
    read as
    if as-rec = x"65E5672C" display "record sequential: UTF-16BE bytes" end-if
    close as
    open output ix
    move n"ﾜ" to ix-key  move n"half" to ix-val  write ix-rec
    move n"あ" to ix-key  move n"hir" to ix-val  write ix-rec
    move n"A" to ix-key  move n"lat" to ix-val  write ix-rec
    close ix
    open input ix
    move "n" to eof
    perform until eof = "y"
        read ix next at end move "y" to eof
            not at end display "indexed: [" ix-key "] [" ix-val "]"
        end-read
    end-perform
    move n"あ" to ix-key
    read ix key is ix-key
    display "by key: " st " [" ix-val "]"
    close ix
    stop run.
