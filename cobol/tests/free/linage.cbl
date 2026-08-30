identification division.
program-id. linage.
*> FD LINAGE: the logical page of a print file -- LINES, FOOTING, TOP,
*> BOTTOM -- the LINAGE-COUNTER register, page overflow, ADVANCING PAGE,
*> and WRITE ... [NOT] AT END-OF-PAGE (EOP).  The page bytes are the
*> ones GnuCOBOL writes (docs/dialect.md); the file is read back below.
environment division.
input-output section.
file-control.
    select prt assign to 'tmp/linage.prn'.
    select back assign to 'tmp/linage.prn'
        organization is line sequential.
data division.
file section.
fd  prt
    linage is 6 lines with footing at 5 lines at top 1 lines at bottom 2.
01  prt-rec  pic x(20).
fd  back.
01  back-rec pic x(20).
working-storage section.
77  i    pic 99.
77  lc   pic 99.
77  n    pic 99 value 0.
77  eof  pic x value 'n'.
procedure division.
main.
    open output prt.
    display 'lc at open ' linage-counter.
    perform varying i from 1 by 1 until i > 9
        move i to prt-rec
        write prt-rec after advancing 1 line
            at end-of-page move linage-counter to lc display 'eop at ' i ' lc=' lc
            not at end-of-page move linage-counter to lc display 'ok  at ' i ' lc=' lc
        end-write
    end-perform.
    move 'PAGE' to prt-rec.
    write prt-rec after advancing page.
    display 'after page lc=' linage-counter.
    move 'two' to prt-rec. write prt-rec after advancing 2 lines.
    display 'after 2 lc=' linage-counter.
    move 'before' to prt-rec. write prt-rec before advancing 3 lines eop display 'eop' not eop display 'no eop' end-write.
    display 'before 3 lc=' linage-counter.
    move 'plain' to prt-rec. write prt-rec.
    display 'plain lc=' linage-counter.
    close prt.
    open input back.
    perform until eof = 'y'
        read back at end move 'y' to eof
            not at end add 1 to n display n ' [' back-rec ']'
        end-read
    end-perform.
    close back.
    stop run.
