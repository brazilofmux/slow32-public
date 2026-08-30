identification division.
program-id. sortfile.
*> Sort-Merge, the file form: an SD, SORT ... USING/GIVING over line
*> sequential files, SORT with INPUT PROCEDURE (RELEASE) and OUTPUT
*> PROCEDURE (RETURN ... AT END / NOT AT END), two keys in opposite
*> directions, and WITH DUPLICATES IN ORDER.  The sort is stable.
environment division.
input-output section.
file-control.
    select in-file assign to 'tmp/sort-in.txt'
        organization is line sequential.
    select out-file assign to 'tmp/sort-out.txt'
        organization is line sequential.
    select back-file assign to 'tmp/sort-out.txt'
        organization is line sequential.
    select work-file assign to 'tmp/sort-work.tmp'.
data division.
file section.
fd  in-file.
01  in-rec           pic x(30).
fd  out-file.
01  out-rec          pic x(30).
fd  back-file.
01  back-rec         pic x(30).
sd  work-file.
01  work-rec.
    05  wr-dept      pic xx.
    05  filler       pic x.
    05  wr-amount    pic s9(5)v99.
    05  filler       pic x.
    05  wr-name      pic x(18).
working-storage section.
01  eof              pic x value 'n'.
01  n                pic 99 value 0.
01  ws-line          pic x(30).
01  total            pic s9(7)v99 value 0.
01  total-ed         pic -(7)9.99.
procedure division.
main.
    open output in-file.
    move 'B2 0010000+ printer paper     ' to in-rec. write in-rec.
    move 'A1 0002550+ coffee            ' to in-rec. write in-rec.
    move 'B2 0000999+ pens              ' to in-rec. write in-rec.
    move 'A1 0010000+ chairs            ' to in-rec. write in-rec.
    move 'C3 0000100- refund            ' to in-rec. write in-rec.
    move 'A1 0010000+ desks             ' to in-rec. write in-rec.
    move 'B2 0010000+ toner             ' to in-rec. write in-rec.
    close in-file.
*>  department ascending, amount descending; equal amounts keep file order
    sort work-file
        on ascending key wr-dept
        on descending key wr-amount
        with duplicates in order
        using in-file
        giving out-file.
    display '--- using/giving'.
    perform show-out.
*>  the procedures: release only the debits, return with a running total
    sort work-file
        on descending key wr-amount wr-name
        input procedure is take-debits
        output procedure is sum-up.
    display 'total ' total-ed.
    display '--- procedures'.
    perform show-out.
    stop run.
take-debits.
    open input in-file.
    move 'n' to eof.
    perform until eof = 'y'
        read in-file at end move 'y' to eof
            not at end
                if in-rec(11:1) = '+'
                    release work-rec from in-rec
                end-if
        end-read
    end-perform.
    close in-file.
sum-up.
    open output out-file.
    move 'n' to eof.
    move 0 to total.
    perform until eof = 'y'
        return work-file into ws-line
            at end move 'y' to eof
            not at end
                add wr-amount to total
                write out-rec from ws-line
        end-return
    end-perform.
    close out-file.
    move total to total-ed.
show-out.
    open input back-file.
    move 'n' to eof.
    move 0 to n.
    perform until eof = 'y'
        read back-file at end move 'y' to eof
            not at end add 1 to n display n ' ' back-rec
        end-read
    end-perform.
    close back-file.
