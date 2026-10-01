*> Micro Focus split keys (-dialect=mf; BP-D2): RECORD KEY IS name = parts
*> and ALTERNATE RECORD KEY IS name = parts WITH DUPLICATES.  Each key is
*> its parts joined in order -- here three numeric parts, which MF allows
*> (its SELECT rule 23).  WRITE out of order, then the prime order back,
*> START and READ by the split names, REWRITE moving a record in the
*> alternate index, DELETE, and a duplicate prime key refused.  What
*> abrignoli_COBSOFT's files do (company + branch + country).
identification division.
program-id. mf-splitkey.
environment division.
input-output section.
file-control.
    select cust assign to "mfsplit.dat"
        organization indexed access dynamic
        record key is cust-key = c-co c-br c-id
        alternate record key is cust-name = c-last c-first with duplicates
        file status is st.
data division.
file section.
fd cust.
01 cust-rec.
   05 c-last  pic x(8).
   05 c-co    pic 99.
   05 c-first pic x(6).
   05 c-br    pic 999.
   05 c-id    pic 9(4).
working-storage section.
01 st pic xx.
01 eof pic x value "n".
procedure division.
    open output cust
    move "smith" to c-last move "ann" to c-first move 2 to c-co move 10 to c-br move 7 to c-id write cust-rec
    move "jones" to c-last move "bob" to c-first move 1 to c-co move 20 to c-br move 3 to c-id write cust-rec
    move "smith" to c-last move "al"  to c-first move 1 to c-co move 10 to c-br move 9 to c-id write cust-rec
    move "brown" to c-last move "cy"  to c-first move 1 to c-co move 10 to c-br move 2 to c-id write cust-rec
    move "dup"   to c-last move "x"   to c-first move 1 to c-co move 10 to c-br move 2 to c-id
    write cust-rec invalid key display "duplicate prime refused, status " st end-write
    close cust
    open i-o cust
    display "-- prime order"
    perform until eof = "y"
        read cust next at end move "y" to eof
            not at end display c-co "-" c-br "-" c-id " " c-last " " c-first
        end-read
    end-perform
    display "-- start cust-key >= 01-020"
    move 1 to c-co move 20 to c-br move 0 to c-id
    start cust key is >= cust-key invalid key display "none" end-start
    read cust next display c-co "-" c-br "-" c-id " " c-last
    display "-- by name"
    move "smith" to c-last move "al" to c-first
    read cust key is cust-name invalid key display "no smith al" end-read
    display c-co "-" c-br "-" c-id " " c-last " " c-first
    move "adams" to c-last
    rewrite cust-rec
    move "brown" to c-last move "cy" to c-first
    read cust key is cust-name invalid key display "no brown" end-read
    delete cust record
    display "-- name order after rewrite and delete"
    move low-values to c-last c-first
    start cust key is >= cust-name
    move "n" to eof
    perform until eof = "y"
        read cust next at end move "y" to eof
            not at end display c-last " " c-first " " c-co "-" c-br "-" c-id
        end-read
    end-perform
    close cust
    stop run.
