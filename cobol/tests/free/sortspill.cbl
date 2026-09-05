identification division.
program-id. sortspill.
*> The external sort: with S32_SORT_MEMORY=24K and S32_SORT_FAN=3 (the
*> .env beside this source) 3,000 released records of 60 bytes do not
*> fit one run, so RELEASE spills sorted runs beside the SD's work file
*> and RETURN reads a two-pass merge of them.  GnuCOBOL sorts in its
*> own way and must print the same lines.  Two keys, opposite
*> directions, duplicates kept in RELEASE order; the first ten, the
*> last ten, and a checksum over all of them.
*> COMP-3 key: default dialect.
environment division.
input-output section.
file-control.
    select work-file assign to 'tmp/sortspill-work.tmp'.
data division.
file section.
sd  work-file.
01  wr.
    05  wr-grp       pic x(3).
    05  wr-amt       pic s9(7)v99 comp-3.
    05  wr-seq       pic 9(5).
    05  wr-pad       pic x(47).
working-storage section.
01  n            pic 9(5) comp value 3000.
01  i            pic 9(9) comp.
01  seed         pic 9(9) comp value 777.
01  t1           pic s9(18) comp.
01  r            pic 9(9) comp.
01  ck           pic 9(9) comp value 0.
01  cnt          pic 9(9) comp value 0.
01  eof          pic x value 'n'.
01  grps         pic x(24) value 'AAABBBCCCDDDEEEFFFGGGHHH'.
01  g            pic 9(4) comp.
01  out-line.
    05  o-grp    pic x(3).
    05  filler   pic x value ' '.
    05  o-amt    pic -(7)9.99.
    05  filler   pic x value ' '.
    05  o-seq    pic 9(5).
01  ck-ed        pic z(9)9.
01  cnt-ed       pic z(9)9.
procedure division.
main.
    sort work-file
        on ascending key wr-grp
        on descending key wr-amt
        with duplicates in order
        input procedure is gen
        output procedure is take
    move ck to ck-ed
    move cnt to cnt-ed
    display 'count=' cnt-ed ' checksum=' ck-ed
    stop run.
gen.
    perform varying i from 1 by 1 until i > n
        compute t1 = seed * 1103515245 + 12345
        compute seed = function mod(t1, 2147483648)
        compute r = seed / 65536
        compute g = function mod(r, 8) * 3 + 1
        move grps(g:3) to wr-grp
        compute t1 = function mod(r, 2000) - 1000
        compute wr-amt = t1 / 4
        move i to wr-seq
        move all '-' to wr-pad
        release wr
    end-perform.
take.
    move 'n' to eof
    perform until eof = 'y'
        return work-file
            at end move 'y' to eof
            not at end
                add 1 to cnt
                compute t1 = ck * 31 + wr-seq
                compute ck = function mod(t1, 1000000007)
                if cnt <= 10 or cnt > 2990
                    move wr-grp to o-grp
                    move wr-amt to o-amt
                    move wr-seq to o-seq
                    display out-line
                end-if
        end-return
    end-perform.
