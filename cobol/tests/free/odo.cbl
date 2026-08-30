*> OCCURS m TO n DEPENDING ON, in gl034/gl040's shape: a table with an
*> ascending key and index-names, grown one entry at a time under a
*> count, walked by PERFORM VARYING on the index, initialised as a
*> group.  COMP-3 amounts: default dialect.
identification division.
program-id. odo.
data division.
working-storage section.
01  yt-entries                   pic 99 value 0.
01  yearly-totals-table.
    05  yearly-totals occurs 0 to 10 times depending on yt-entries
        ascending key is yt-year indexed by yt-idx, yt-jdx.
        10  yt-year              pic 9(4).
        10  yt-debits            pic 9(9)v99 comp-3.
        10  yt-credits           pic 9(9)v99 comp-3.
01  ws-year     pic 9(4).
01  ws-amt      pic 9(9)v99 comp-3.
01  i           pic 99 comp.
procedure division.
    initialize yearly-totals-table.
    move 0 to yt-entries.
    perform varying i from 1 by 1 until i > 4
        add 1 to yt-entries
        compute ws-year = 2020 + i
        move ws-year to yt-year(yt-entries)
        compute ws-amt = i * 100.5
        move ws-amt to yt-debits(yt-entries)
        move 0 to yt-credits(yt-entries)
    end-perform.
    display 'entries=' yt-entries.
    perform varying yt-idx from 1 by 1 until yt-idx > yt-entries
        display yt-year(yt-idx) ' ' yt-debits(yt-idx) ' ' yt-credits(yt-idx)
    end-perform.
    set yt-idx to 2.
    add 50 to yt-credits(yt-idx).
    set yt-idx up by 1.
    move 7 to yt-credits(yt-idx).
    perform varying yt-jdx from 1 by 1 until yt-jdx > yt-entries
        if yt-debits(yt-jdx) not = yt-credits(yt-jdx)
            display 'open ' yt-year(yt-jdx)
        end-if
    end-perform.
    initialize yearly-totals-table.
    display '[' yt-year(1) '] ' yt-debits(1).
    stop run.
