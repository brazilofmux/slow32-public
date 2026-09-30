*> ALPHABET ... IS EBCDIC (the ruling of 2026-09-28; docs/dialect.md):
*> as the PROGRAM COLLATING SEQUENCE lowercase comes before uppercase and
*> letters before digits, the reverse of ASCII; a SORT orders by it; and
*> as an FD's CODE-SET the records are EBCDIC (code page 037) on the
*> medium -- read back raw they show 200 197 211 211 214 (C8 C5 D3 D3 D6)
*> for HELLO, 96 (60, the minus sign) 240 244 242 for -042 -- and
*> native again through the CODE-SET file.  EBCDIC is an implementor-name
*> in the 1985 text: the oracle compiles it in its default dialect.
identification division.
program-id. ebcdic.
environment division.
configuration section.
object-computer. slow32 program collating sequence is eb.
special-names.
    alphabet eb is ebcdic.
input-output section.
file-control.
    select ef assign to "ebcdic.dat" organization sequential.
    select raw-f assign to "ebcdic.dat" organization sequential.
    select sf assign to "ebcdic.srt".
data division.
file section.
fd  ef code-set is eb.
01  e-rec.
    05 e-text pic x(5).
    05 e-num  pic s9(3) sign leading separate.
fd  raw-f.
01  r-rec pic x(9).
sd  sf.
01  s-rec pic x(3).
working-storage section.
01  lo-a   pic x value "a".
01  up-a   pic x value "A".
01  one    pic x value "1".
01  i      pic 99.
01  codes  pic x(40) value spaces.
01  p      pic 99.
01  cv     pic 999.
01  tbl.
    05 t-e pic x(3) occurs 4.
procedure division.
    *> items, not two literals: a relation needs a variable (8.8.4.2.1)
    if lo-a < up-a display "a before A" else display "A before a" end-if
    if up-a < one display "A before 1" else display "1 before A" end-if
    open output ef
    move "HELLO" to e-text  move -42 to e-num
    write e-rec
    close ef
    open input raw-f
    read raw-f
    move 1 to p
    perform varying i from 1 by 1 until i > 9
        compute cv = function ord(r-rec(i:1)) - 1
        string cv " " delimited by size into codes with pointer p
    end-perform
    display "raw codes: " codes
    close raw-f
    open input ef
    read ef
    display "through CODE-SET: [" e-text "] " e-num
    close ef
    move "ab1" to t-e(1)  move "AB2" to t-e(2)  move "123" to t-e(3)  move "xyz" to t-e(4)
    sort sf on ascending key s-rec collating sequence is eb
        input procedure gen output procedure out
    stop run.
gen.
    perform varying i from 1 by 1 until i > 4
        move t-e(i) to s-rec  release s-rec
    end-perform.
out.
    perform 4 times
        return sf at end continue not at end display "sorted: " s-rec end-return
    end-perform.
