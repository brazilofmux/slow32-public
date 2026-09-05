identification division.
program-id. bidx.
*> The B+tree key file under pressure: 3,000 records with two alternate
*> keys (one WITH DUPLICATES) written in shuffled key order, then random
*> READs by every key, START + READ NEXT scans along each key, DELETEs
*> and REWRITEs that move an alternate key, then a full scan and a
*> checksum -- under S32_INDEX_CACHE=16 (the .env), so pages are evicted
*> and reread throughout.  GnuCOBOL (BDB) prints the same lines but one:
*> a random READ whose next record shares the alternate key returns 02
*> here (the 1985 I-O status rule) and 00 there -- the documented
*> divergence, as in altkey (idxbig.oracle-expected).
*> COMP keys: default dialect.
environment division.
input-output section.
file-control.
    select ixf assign to 'bench/tmp/bidx.dat'
        organization indexed access dynamic
        record key ix-key
        alternate record key ix-grp with duplicates
        alternate record key ix-code
        file status fs.
data division.
file section.
fd  ixf.
01  ix-rec.
    05  ix-key      pic 9(6).
    05  ix-grp      pic x(3).
    05  ix-code     pic 9(6).
    05  ix-amt      pic s9(7)v99.
    05  ix-pad      pic x(20).
working-storage section.
01  fs              pic xx.
01  n               pic 9(9) comp value 3000.
01  arg             pic x(12).
01  i               pic 9(9) comp.
01  seed            pic 9(9) comp value 4242.
01  t1              pic s9(18) comp.
01  r               pic 9(9) comp.
01  k               pic 9(9) comp.
01  ck              pic 9(9) comp value 0.
01  cnt             pic 9(9) comp value 0.
01  eof             pic x value 'n'.
01  grps            pic x(15) value 'REDGRNBLUYELWHT'.
01  g               pic 9(4) comp.
01  ck-ed           pic z(9)9.
01  cnt-ed          pic z(9)9.
01  amt-ed          pic -(7)9.99.
procedure division.
main.
    accept arg from command-line
    if arg not = spaces compute n = function numval(arg) end-if
    open output ixf
    perform load
    close ixf
    open i-o ixf
    perform random-reads
    perform scan-grp
    perform scan-code
    perform churn
    perform final-scan
    close ixf
    move ck to ck-ed
    move cnt to cnt-ed
    display 'records=' cnt-ed ' checksum=' ck-ed
    stop run.
next-r.
    compute t1 = seed * 1103515245 + 12345
    compute seed = function mod(t1, 2147483648)
    compute r = seed / 65536.
load.
*> keys are a permutation of 1..n: k = (i * 7919) mod n + 1 (n and 7919 coprime)
    perform varying i from 1 by 1 until i > n
        compute k = function mod(i * 7919, n) + 1
        move k to ix-key
        perform next-r
        compute g = function mod(r, 5) * 3 + 1
        move grps(g:3) to ix-grp
        compute ix-code = 900000 - k
        compute t1 = function mod(r, 200000) - 100000
        compute ix-amt = t1 / 100
        move all '.' to ix-pad
        write ix-rec
        if fs not = '00' and fs not = '02' display 'write fs=' fs ' key=' ix-key end-if
    end-perform.
random-reads.
    perform 200 times
        perform next-r
        compute k = function mod(r, n) + 1
        move k to ix-key
        read ixf
        if fs not = '00' display 'read key ' ix-key ' fs=' fs end-if
        compute t1 = ck * 31 + ix-code
        compute ck = function mod(t1, 1000000007)
        compute ix-code = 900000 - k
        read ixf key is ix-code
        if fs not = '00' or ix-key not = k display 'read alt code ' ix-code ' fs=' fs end-if
    end-perform
    move 'BLU' to ix-grp
    read ixf key is ix-grp
    display 'first BLU: key=' ix-key ' fs=' fs.
scan-grp.
    move 'GRN' to ix-grp
    start ixf key is >= ix-grp
    display 'start GRN fs=' fs
    move 0 to cnt
    move 'n' to eof
    perform until eof = 'y'
        read ixf next at end move 'y' to eof
            not at end
                if ix-grp not = 'GRN' move 'y' to eof
                else
                    add 1 to cnt
                    compute t1 = ck * 31 + ix-key
                    compute ck = function mod(t1, 1000000007)
                end-if
        end-read
    end-perform
    move cnt to cnt-ed
    display 'GRN count=' cnt-ed.
scan-code.
    move 899000 to ix-code
    start ixf key is > ix-code
    display 'start code > 899000 fs=' fs
    move 0 to cnt
    move 'n' to eof
    perform until eof = 'y'
        read ixf next at end move 'y' to eof
            not at end
                add 1 to cnt
                if cnt <= 3 display '  code ' ix-code ' key ' ix-key end-if
        end-read
    end-perform
    move cnt to cnt-ed
    display 'codes above 899000=' cnt-ed.
churn.
*> delete every 7th key, rewrite every 11th moving it to group WHT
    perform varying k from 7 by 7 until k > n
        move k to ix-key
        delete ixf record
        if fs not = '00' display 'delete ' ix-key ' fs=' fs end-if
    end-perform
    perform varying k from 11 by 11 until k > n
        move k to ix-key
        read ixf
        if fs = '00'
            move 'WHT' to ix-grp
            rewrite ix-rec
            if fs not = '00' and fs not = '02' display 'rewrite ' ix-key ' fs=' fs end-if
        end-if
    end-perform
*> a deleted key reads as 23; writing it back reuses a slot
    move 14 to ix-key
    read ixf
    display 'read deleted 14 fs=' fs
    move 14 to ix-key move 'RED' to ix-grp move 899986 to ix-code move 1.25 to ix-amt move all '+' to ix-pad
    write ix-rec
    display 'rewrite deleted 14 fs=' fs.
final-scan.
    move 0 to cnt
    move 000000 to ix-key
    start ixf key is >= ix-key
    move 'n' to eof
    perform until eof = 'y'
        read ixf next at end move 'y' to eof
            not at end
                add 1 to cnt
                compute t1 = ck * 31 + ix-key
                compute ck = function mod(t1, 1000000007)
                if ix-grp = 'WHT' compute t1 = ck + 7 compute ck = function mod(t1, 1000000007) end-if
        end-read
    end-perform.
