*> The small 2023 statements (docs/plans/standard-queue.md item 31):
*> USAGE PACKED-DECIMAL WITH NO SIGN (13.18.60 GR 25: no sign nibble, the
*> standard's COMP-6); DELETE FILE [OVERRIDE] file ... (14.9.10 format 2:
*> 00, 05 when the file is not there, 41 when open, an indexed file's key
*> file removed with it, several files in one statement, ON EXCEPTION);
*> CONTINUE AFTER expression SECONDS (14.9.9: a hundredth, zero, a
*> negative value zero and EC-CONTINUE-LESS-THAN-ZERO, nonfatal); WRITE
*> with both BEFORE and AFTER ADVANCING (14.9.51) on a print file and a
*> LINAGE file; GOBACK WITH ERROR STATUS 7 ends the run with that status
*> (14.9.18 GR 3; stmts2023.exitcode) after a called program's own GOBACK
*> WITH STATUS returned to its caller.  No oracle: GnuCOBOL 4 has none of
*> these but DELETE FILE, which it spells differently.
*> docs/conformance/edition-2023.md
identification division.
program-id. stmts2023.
environment division.
input-output section.
file-control.
    select f assign to "stmts2023a.dat" organization sequential file status fs.
    select g assign to "stmts2023b.dat" organization indexed access dynamic record key gk file status gs.
    select p assign to "stmts2023.prn".
    select l assign to "stmts2023.lin" organization line sequential.
data division.
file section.
fd f.
01 frec pic x(10).
fd g.
01 grec.
   05 gk pic x(3).
   05 gv pic x(5).
fd p.
01 prec pic x(5).
fd l linage is 10 lines.
01 lrec pic x(5).
working-storage section.
01 fs pic xx.
01 gs pic xx.
01 p6 pic 9(5) packed-decimal with no sign value 12345.
01 p3 pic s9(5) packed-decimal value -12345.
01 t pic 9(3)v99 value 0.
01 line-in pic x(10).
procedure division.
declaratives.
d section.
    use after exception condition ec-continue-less-than-zero.
d1.
    display "  EC-CONTINUE-LESS-THAN-ZERO".
end declaratives.
main section.
m1.
    display "p6 " p6 " " function length(p6) "; p3 " p3 " " function length(p3)
    add 1 to p6 display p6
    open output f write frec from "hello" close f
    open output g move "k01v0001" to grec write grec close g
    delete file f on exception display "unexpected" not on exception display "deleted f: " fs end-delete
    delete file f display "deleted again: " fs
    open output f close f
    open input f
    delete file f display "while open: " fs
    close f
    delete file override f g display "both: " fs " " gs
    open input g display "open after delete: " gs
    move 0.01 to t
    continue after t seconds
    continue after 0 seconds
    continue
    display "continued"
    >>turn ec-continue-less-than-zero checking on
    continue after -1 seconds
    display "nonfatal: continues" " " function exception-status
    >>turn ec-continue-less-than-zero checking off
    open output p l
    write prec from "one" after advancing 1 line
    write prec from "two" after advancing 2 lines before advancing 1 line
    write prec from "three" before 2 after 1
    write prec from "four" after 1
    write lrec from "one" after advancing 1 line
    write lrec from "two" after advancing 2 lines before advancing 1 line
    write lrec from "three" after 1
    close p l
    perform show-file
    call "stmts2023-sub"
    display "back from the sub"
    goback with error status 7.
show-file.
    open input p
    perform until exit
        read p into line-in at end exit perform end-read
        display "p: [" line-in "]"
    end-perform
    close p
    open input l
    perform until exit
        read l into line-in at end exit perform end-read
        display "l: [" line-in "]"
    end-perform
    close l.
end program stmts2023.
identification division.
program-id. stmts2023-sub.
procedure division.
    goback with error status 9.
end program stmts2023-sub.
