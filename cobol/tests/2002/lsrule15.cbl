identification division.
program-id. lsrule15.
*> A LINE SEQUENTIAL line longer than the record (2023 14.9.30 rule 15;
*> cobol ISSUES-94 N8): the record is filled, the status is 06, and the
*> rest of the line is left for the next READ. A line exactly the record's
*> length is 00, and so is one ending CR LF (the CR is the line end's,
*> not a fifth character). For national records the line is UTF-8 and a record of
*> n positions takes n code units: a character past U+FFFF needs two, so
*> "ab" and a smiling face do not fit in three -- the READ stops before
*> the face and the next one begins with it. Under -std=85 (GnuCOBOL's
*> behaviour, which majesty reads) the rest is dropped and the status is
*> 04; that path is unchanged.
*> No oracle: GnuCOBOL 4 gives 04 and drops the rest.
environment division.
input-output section.
file-control.
    select wide-a assign to "tmp/r15a.txt" organization is line sequential.
    select narrow-a assign to "tmp/r15a.txt" organization is line sequential
        file status is sa.
    select wide-n assign to "tmp/r15n.txt" organization is line sequential.
    select narrow-n assign to "tmp/r15n.txt" organization is line sequential
        file status is sn.
data division.
file section.
fd  wide-a.
01  wa  pic x(20).
fd  narrow-a.
01  na  pic x(4).
fd  wide-n.
01  wn  pic n(10).
fd  narrow-n.
01  nn  pic n(3).
working-storage section.
01  sa  pic xx.
01  sn  pic xx.
procedure division.
    open output wide-a
    move "abcdefgh" to wa write wa
    move "xy" to wa write wa
    move "1234" to wa write wa
    move "ABCDEFGHIJ" to wa write wa
    move spaces to wa
    string "wxyz" x"0D" delimited by size into wa
    write wa
    close wide-a
    open input narrow-a
    perform until sa = "10"
        read narrow-a
        if sa not = "10" display "alnum [" na "] " sa else display "alnum end " sa end-if
    end-perform
    close narrow-a
    open output wide-n
    move n"日本語テスト" to wn write wn
    move n"abc" to wn write wn
    move n"ab😀c" to wn write wn
    close wide-n
    open input narrow-n
    perform until sn = "10"
        read narrow-n
        if sn not = "10" display "national [" function display-of(nn) "] " sn else display "national end " sn end-if
    end-perform
    close narrow-n
    stop run.
