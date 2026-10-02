identification division.
program-id. seqbyteec.
*> The I-O status that EC-I-O checking reads is the last statement's,
*> and the short entries of READ and WRITE (libcob.c) are statements
*> too.  A successful statement is asked for its status only under
*> EC-I-O-WARNING checking (a status 0x that is not 00), so that is
*> what is on: an OPTIONAL file that is not there opens with 05 and
*> raises the warning, and the WRITE or READ of a one-byte or a
*> four-byte record that follows on another file must leave a status
*> of its own -- 00 -- and raise nothing.  Then a READ past the end
*> with no phrase, which does raise.  No oracle (ecraise).
environment division.
input-output section.
file-control.
    select optional o1 assign to "tmp/seqbyteec-none.dat"
        organization sequential.
    select b1 assign to "tmp/seqbyteec-b.dat" organization sequential.
    select b4 assign to "tmp/seqbyteec-c.dat" organization sequential.
data division.
file section.
fd  o1.
01  o1-rec       pic x(4).
fd  b1.
01  b1-rec       pic x.
fd  b4.
01  b4-rec       pic x(4).
working-storage section.
01  k            pic 99.
01  ef           pic x(40).
procedure division.
declaratives.
io-any section.
    use after exception condition ec-i-o.
i1.
    move function exception-file to ef
    display "  EC-I-O declarative: " function exception-status(1:14)
            " status " ef(1:2).
end declaratives.
main section.
m1.
>>TURN EC-I-O CHECKING ON
>>TURN EC-I-O-WARNING CHECKING ON
    open output b1 b4
    *> the first WRITE of each file goes the long way; these are the rest
    write b1-rec from "1"
    write b4-rec from "four"
    perform varying k from 1 by 1 until k > 2
        display "open, 05:"
        open input o1
        write b1-rec from "2"
        display "a byte written"
        close o1
        open input o1
        write b4-rec from "more"
        display "four written"
        close o1
    end-perform
    close b1 b4
    open input b1 b4
    read b1  read b4
    perform varying k from 1 by 1 until k > 2
        open input o1
        read b1
        display "a byte read: " b1-rec
        close o1
        open input o1
        read b4
        display "four read: " b4-rec
        close o1
    end-perform
    display "past the end of b1, no phrase:"
    read b1
    close b1 b4
    display "done"
    stop run.
