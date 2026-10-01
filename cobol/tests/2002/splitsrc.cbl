*> A record key SOURCE IS its parts (2002 12.3.4.12): record-key-name-1
*> is the concatenation of the data-names in the order written (general
*> rule 2), alphanumeric parts of one category (syntax rule 2), named by
*> READ ... KEY and START ... KEY (14.8.29, 14.8.37).  The ALTERNATE
*> RECORD KEY takes the same phrase.  Micro Focus writes "=" for it
*> (free/mf-splitkey).
identification division.
program-id. splitsrc.
environment division.
input-output section.
file-control.
    select parts assign to "splitsrc.dat"
        organization indexed access dynamic
        record key is part-key source is p-group p-num
        alternate record key is part-desc source is p-name p-group with duplicates.
data division.
file section.
fd parts.
01 part-rec.
   05 p-name  pic x(10).
   05 p-num   pic x(4).
   05 p-group pic x(2).
   05 p-price pic 9(5).
working-storage section.
01 eof pic x value "n".
procedure division.
    open output parts
    move "washer" to p-name move "0003" to p-num move "zz" to p-group move 15 to p-price write part-rec
    move "bolt"   to p-name move "0001" to p-num move "aa" to p-group move 40 to p-price write part-rec
    move "nut"    to p-name move "0002" to p-num move "aa" to p-group move 10 to p-price write part-rec
    move "bolt"   to p-name move "0009" to p-num move "mm" to p-group move 45 to p-price write part-rec
    close parts
    open input parts
    display "-- by part-key (group, number)"
    perform until eof = "y"
        read parts next at end move "y" to eof
            not at end display p-group p-num " " p-name " " p-price
        end-read
    end-perform
    display "-- by part-desc from bolt"
    move "bolt" to p-name move spaces to p-group
    start parts key is >= part-desc
    move "n" to eof
    perform 3 times
        read parts next at end move "y" to eof
            not at end display p-name " " p-group p-num
        end-read
    end-perform
    move "aa" to p-group move "0002" to p-num
    read parts key is part-key invalid key display "not found" end-read
    display "random read: " p-name
    close parts
    stop run.
