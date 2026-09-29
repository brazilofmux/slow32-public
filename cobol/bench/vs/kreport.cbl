*> kreport -- the batch shape: 300,000 transactions generated to a file,
*> SORTed USING/GIVING by account, then read with a control break per
*> account, totals edited into a print file.
identification division.
program-id. kreport.
environment division.
input-output section.
file-control.
    select tx  assign to "krep-tx.dat" organization sequential.
    select srt assign to "krep-sort.tmp".
    select sx  assign to "krep-sorted.dat" organization sequential.
    select prt assign to "krep.prn" organization line sequential.
data division.
file section.
fd  tx.
01  tx-rec.
    05  tx-acct  pic 9(6).
    05  tx-date  pic 9(8).
    05  tx-amt   pic s9(7)v99 comp-3.
    05  tx-desc  pic x(30).
sd  srt.
01  s-rec.
    05  s-acct   pic 9(6).
    05  s-date   pic 9(8).
    05  s-amt    pic s9(7)v99 comp-3.
    05  s-desc   pic x(30).
fd  sx.
01  sx-rec.
    05  sx-acct  pic 9(6).
    05  sx-date  pic 9(8).
    05  sx-amt   pic s9(7)v99 comp-3.
    05  sx-desc  pic x(30).
fd  prt.
01  prt-line pic x(60).
working-storage section.
01  n        pic 9(9) comp value 300000.
01  i        pic 9(9) comp.
01  eof      pic x value "n".
01  cur      pic 9(6) value 0.
01  acc-tot  pic s9(11)v99 comp-3 value 0.
01  grand    pic s9(13)v99 comp-3 value 0.
01  accts    pic 9(9) comp value 0.
01  pl.
    05  pl-acct  pic 9(6).
    05  filler   pic x(4) value spaces.
    05  pl-tot   pic -,---,---,--9.99.
    05  filler   pic x(4) value spaces.
    05  pl-cnt   pic zzz,zz9.
01  cnt      pic 9(9) comp value 0.
procedure division.
    open output tx
    perform varying i from 1 by 1 until i > n
        compute tx-acct = function mod(i * 7919, 5000) + 100000
        compute tx-date = 20260101 + function mod(i, 28)
        compute tx-amt = function mod(i * 37, 200000) / 100 - 500
        move "TRANSACTION DESCRIPTION" to tx-desc
        write tx-rec
    end-perform
    close tx
    sort srt on ascending key s-acct s-date using tx giving sx
    open input sx output prt
    perform until eof = "y"
        read sx at end move "y" to eof
            not at end
                if sx-acct not = cur and cur not = 0 perform break end-if
                move sx-acct to cur
                add sx-amt to acc-tot
                add 1 to cnt
        end-read
    end-perform
    perform break
    close sx prt
    display "kreport " accts " " grand
    stop run.
break.
    move cur to pl-acct
    move acc-tot to pl-tot
    move cnt to pl-cnt
    write prt-line from pl
    add acc-tot to grand
    add 1 to accts
    move 0 to acc-tot cnt.
