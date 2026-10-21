*> SORT with COLLATING SEQUENCE: the alphabet is an alphanumeric
*> collating sequence, so it orders the alphanumeric keys and leaves a
*> national key in the native national order, its code units (2023
*> 14.9.40.4 general rule 5).  A file SORT and a table SORT, each on a
*> national key and on an alphanumeric key under the same alphabet.
*> No oracle: GnuCOBOL's USAGE NATIONAL is unfinished and refuses this
*> program; the order is the rule's, checked by hand.
identification division.
program-id. natsortcoll.
environment division.
configuration section.
special-names.
    alphabet flip is "y" "x".
input-output section.
file-control.
    select sort-file assign to "natsortcoll.tmp".
data division.
file section.
sd sort-file.
01 sort-rec.
   05 s-nat pic n(1).
   05 s-alpha pic x(1).
working-storage section.
01 rows.
   05 row occurs 4.
      10 r-nat pic n(1).
      10 r-alpha pic x(1).
01 i pic 9.
01 line-out pic x(12).
procedure division.
    move n"y" to r-nat(1) move "w" to r-alpha(1)
    move n"é" to r-nat(2) move "x" to r-alpha(2)
    move n"w" to r-nat(3) move "y" to r-alpha(3)
    move n"x" to r-nat(4) move "v" to r-alpha(4)
    display "file, national key:"
    sort sort-file on ascending key s-nat
        collating sequence is flip
        input procedure feed
        output procedure drain
    display "file, alphanumeric key:"
    sort sort-file on ascending key s-alpha
        collating sequence is flip
        input procedure feed
        output procedure drain
    display "table, national key:"
    sort row ascending r-nat collating sequence is flip
    perform show-rows
    display "table, alphanumeric key:"
    sort row ascending r-alpha collating sequence is flip
    perform show-rows
    stop run.
feed.
    perform varying i from 1 by 1 until i > 4
        move r-nat(i) to s-nat
        move r-alpha(i) to s-alpha
        release sort-rec
    end-perform.
drain.
    perform 4 times
        return sort-file at end exit perform end-return
        move spaces to line-out
        string "  " function display-of(s-nat) " " s-alpha
            delimited by size into line-out
        display function trim(line-out trailing)
    end-perform.
show-rows.
    perform varying i from 1 by 1 until i > 4
        move spaces to line-out
        string "  " function display-of(r-nat(i)) " " r-alpha(i)
            delimited by size into line-out
        display function trim(line-out trailing)
    end-perform.
