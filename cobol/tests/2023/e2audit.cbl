*> 2023 Annex E.2 audit (docs/plans/standard-queue.md item 36): the
*> behaviour rows probed -- item 11 (ALL literal with no length from the
*> context: the literal's), item 19a (an invalid key with no INVALID KEY
*> phrase runs the open mode's declarative), item 30 (the END-OF-PAGE
*> condition without the phrase: the WRITE completes), and item 16 (07 only
*> from CLOSE). No oracle: GnuCOBOL 4 has no -std=cobol2023.
identification division.
program-id. e2audit.
environment division.
input-output section.
file-control.
    select prt assign to "e2audit-prt.txt" organization line sequential file status fs.
    select ixf assign to "e2audit-ix.dat" organization indexed access random record key ix-key file status fs2.
data division.
file section.
fd prt linage 3 lines.
01 prt-rec pic x(10).
fd ixf.
01 ix-rec.
   05 ix-key pic x(3).
   05 ix-data pic x(5).
working-storage section.
01 fs pic xx.
01 fs2 pic xx.
01 n pic 9 value 0.
procedure division.
declaratives.
io-sec section.
    use after error procedure on i-o.
io-para.
    display "declarative for I-O: " fs2.
end declaratives.
main section.
    display "[" all "ab" "]" "[" all "abc" "-" all "x" "-" zero "-" space "]".
    move all "xy" to prt-rec.
    display "[" prt-rec "]".
    open output prt.
    perform 4 times
        add 1 to n
        move n to prt-rec
        write prt-rec
        display "write " n " fs=" fs " linage-counter=" linage-counter of prt
    end-perform.
    close prt.
    display "close: " fs.
    open output ixf.
    move "k01" to ix-key.
    write ix-rec.
    display "write: " fs2.
    close ixf.
    open i-o ixf.
    move "k02" to ix-key.
    rewrite ix-rec.
    display "after rewrite without INVALID KEY: " fs2.
    close ixf.
    stop run.
