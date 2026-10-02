*> FD CODE-SET over more than one record (EBCDIC is an implementor-name
*> in the 1985 text: the oracle compiles it in its default dialect).
*> The runtime finds out about a fixed-length sequential file at its
*> first record and takes a short way with the rest (libcob.c, "The
*> short entries of READ and WRITE"); a file with a CODE-SET is not one
*> of those -- every record is translated, the second and the fifth as
*> the first.  One-byte records and five-byte ones, written through the
*> CODE-SET, read back raw (193 is EBCDIC A, 240 is 0) and read back
*> through it.
identification division.
program-id. codesetrecs.
environment division.
configuration section.
special-names.
    alphabet eb is ebcdic.
input-output section.
file-control.
    select c1 assign to "tmp/codesetrecs1.dat" organization sequential.
    select r1 assign to "tmp/codesetrecs1.dat" organization sequential.
    select c5 assign to "tmp/codesetrecs5.dat" organization sequential.
    select r5 assign to "tmp/codesetrecs5.dat" organization sequential.
data division.
file section.
fd  c1 code-set is eb.
01  c1-rec       pic x.
fd  r1.
01  r1-rec       pic x.
fd  c5 code-set is eb.
01  c5-rec       pic x(5).
fd  r5.
01  r5-rec       pic x(5).
working-storage section.
01  i            pic 99.
01  j            pic 99.
01  letters      pic x(6) value "ABCDE0".
01  cv           pic 999.
01  line-out     pic x(60).
01  p            pic 99.
procedure division.
    open output c1 c5
    perform varying i from 1 by 1 until i > 6
        move letters(i:1) to c1-rec
        write c1-rec
        move all "x" to c5-rec
        move letters(i:1) to c5-rec(1:1) c5-rec(5:1)
        write c5-rec
    end-perform
    close c1 c5
    move spaces to line-out  move 1 to p
    open input r1
    perform varying i from 1 by 1 until i > 6
        read r1
        compute cv = function ord(r1-rec) - 1
        string cv " " delimited by size into line-out with pointer p
    end-perform
    close r1
    display "one byte, raw: " line-out
    open input r5
    perform varying i from 1 by 1 until i > 6
        read r5
        move spaces to line-out  move 1 to p
        perform varying j from 1 by 1 until j > 5
            compute cv = function ord(r5-rec(j:1)) - 1
            string cv " " delimited by size into line-out with pointer p
        end-perform
        display "five bytes, raw: " line-out(1:20)
    end-perform
    close r5
    move spaces to line-out
    open input c1
    perform varying i from 1 by 1 until i > 6
        read c1
        move c1-rec to line-out(i:1)
    end-perform
    close c1
    display "one byte, through CODE-SET: " line-out(1:6)
    open input c5
    perform varying i from 1 by 1 until i > 6
        read c5
        display "five bytes, through CODE-SET: " c5-rec
    end-perform
    close c5
    stop run.
