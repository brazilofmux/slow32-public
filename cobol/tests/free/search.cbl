*> SEARCH: the serial form with several WHENs, VARYING and AT END, and
*> SEARCH ALL on keyed tables in gl042's and gl034's shapes -- the
*> latter an OCCURS DEPENDING ON table grown under a count.  The
*> VARYING item starts equal to the index: the text says only that it
*> is incremented in step, and starting it elsewhere shows GnuCOBOL
*> reporting the index's value.
identification division.
program-id. search.
data division.
working-storage section.
01  class-initialized.
    05  filler pic x(14) value 'AAssets'.
    05  filler pic x(14) value 'LLiabilities'.
    05  filler pic x(14) value 'QEquity'.
01  class-table redefines class-initialized.
    05  class-entry occurs 3 times ascending key is class-letter
        indexed by cl.
        10  class-letter pic x.
        10  class-name   pic x(13).
01  ws-current-class      pic x.
01  ws-current-class-name pic x(13).
01  nums.
    05  n occurs 6 times indexed by ix pic 99.
01  pos   pic 99 comp.
01  yt-entries pic 99 value 0.
01  yearly-totals-table.
    05  yearly-totals occurs 0 to 10 times depending on yt-entries
        ascending key is yt-year indexed by yt-idx.
        10  yt-year   pic 9(4).
        10  yt-count  pic 99.
01  ws-year pic 9(4).
01  i       pic 99 comp.
procedure division.
    move 'A' to ws-current-class.
    perform lookup-class.
    move 'Q' to ws-current-class.
    perform lookup-class.
    move 'X' to ws-current-class.
    perform lookup-class.
*> serial SEARCH with VARYING, two WHENs, AT END
    move 11 to n(1). move 22 to n(2). move 33 to n(3).
    move 44 to n(4). move 55 to n(5). move 66 to n(6).
    set ix to 1.
    move 1 to pos.
    search n varying pos
        at end display 'no match'
        when n(ix) = 44 display 'found 44 at ' pos
        when n(ix) > 50 display 'first over 50 at ' pos
    end-search.
    set ix to 1.
    move 1 to pos.
    search n varying pos
        at end display 'no match'
        when n(ix) > 50 display 'first over 50 at ' pos
    end-search.
    set ix to 1.
    search n
        at end display 'nothing over 90'
        when n(ix) > 90 display 'x'
    end-search.
    set ix to 4.
    search n
        at end display 'no'
        when n(ix) < 30 display 'x'
        when n(ix) = 55 display 'from 4: 55 at index'
    end-search.
*> SEARCH ALL on the growing table
    perform varying i from 1 by 1 until i > 3
        compute ws-year = 2019 + i * 2
        perform update-yt
    end-perform.
    move 2021 to ws-year.
    perform update-yt.
    move 2025 to ws-year.
    perform update-yt.
    perform varying yt-idx from 1 by 1 until yt-idx > yt-entries
        display yt-year(yt-idx) ' ' yt-count(yt-idx)
    end-perform.
    stop run.
lookup-class.
    search all class-entry
        at end move 'unknown' to ws-current-class-name
        when class-letter(cl) = ws-current-class
            move class-name(cl) to ws-current-class-name.
    display '[' ws-current-class '] ' ws-current-class-name.
update-yt.
    search all yearly-totals
        at end perform insert-yt
        when yt-year(yt-idx) = ws-year
            add 1 to yt-count(yt-idx).
insert-yt.
    add 1 to yt-entries.
    move ws-year to yt-year(yt-entries).
    move 1 to yt-count(yt-entries).
