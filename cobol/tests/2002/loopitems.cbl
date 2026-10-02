identification division.
program-id. loopitems.
*> An in-line loop's binary items are kept in registers while the loop
*> runs (src/cobc/loopreg.h) -- unless something in the loop can change
*> one of them some other way than by storing into it by name.  Each
*> loop here does exactly that on its second pass, and counts its
*> passes: through a REDEFINES; through the group, moved to, initialized,
*> and one byte of it; through a table laid over the item; in a
*> performed paragraph; by READ INTO, by READ into the record area the
*> bound is in, by the FILE STATUS a WRITE leaves; by a store the
*> runtime makes (ROUNDED, STRING's POINTER); through a BASED item set to
*> its address, alone and as a group; as an element of a table of long
*> elements, which is a call; as the RELATIVE KEY of a file read in the
*> loop, and as a LINAGE-COUNTER.  Then the plain cases the register
*> follows: the bound lowered in the body, a store that wraps, a
*> one-byte item past its picture and past its byte, loops four deep.
environment division.
input-output section.
file-control.
    select f1 assign to "tmp/loopitems.dat" organization sequential.
    select f2 assign to "tmp/loopitems2.dat" organization sequential
        file status is st.
    select relf assign to "tmp/loopitems.rel" organization relative
        access sequential relative key RK.
    select prf assign to "tmp/loopitems.prt" organization line sequential.
data division.
file section.
fd  f1.
01  f1-rec.
    05  f1-n     pic 9(4) comp.
    05  f1-m     pic 9(4) comp.
fd  f2.
01  f2-rec       pic x(4).
fd  relf.
01  relf-rec       pic x(4).
fd  prf linage is 6 lines.
01  prf-rec       pic x(4).
working-storage section.
01  G.
    05  I        pic 9(4) comp.
    05  N        pic 9(4) comp.
01  GX redefines G.
    05  I-X      pic xx.
    05  filler   pic xx.
01  GT redefines G.
    05  T        pic 9(4) comp occurs 2.
01  GZ.
    05  filler   pic 9(4) comp value 4.
    05  filler   pic 9(4) comp value 6.
01  ST-AREA.
    05  st       pic xx.
01  ST-N redefines ST-AREA pic 9(4) comp.
01  PASSES       pic 9(4) comp.
01  K            pic 9(4) comp.
01  P            pic 9(4) comp.
01  B1           pic 9(2) comp.
01  BS           binary-short signed.
01  SUM1         pic s9(9) comp.
01  TXT          pic x(12).
01  J            pic 9(4) comp.
01  L            pic 9(4) comp.
01  RK           pic 9(4) comp.
01  BC           binary-char unsigned.
01  LK           pic 9(4) comp based.
01  LKG based.
    05  LKG1     pic 9(4) comp.
    05  LKG2     pic 9(4) comp.
01  BIG.
    05  BI       pic 9(4) comp.
    05  filler   pic x(38).
01  BT redefines BIG.
    05  BE       pic x(20) occurs 2.
01  X20.
    05  filler   pic 9(4) comp value 4.
    05  filler   pic x(18) value spaces.
procedure division.
main-para.
    open output f1
    move 2 to f1-n  move 9 to f1-m  write f1-rec
    move 3 to f1-n  move 4 to f1-m  write f1-rec
    move 1 to f1-n  move 8 to f1-m  write f1-rec
    close f1

    move 0 to PASSES
    perform varying I from 1 by 1 until I > 5 or PASSES > 8
        add 1 to PASSES
        if PASSES = 2 move x"0003" to I-X end-if
    end-perform
    display "a redefinition:      " PASSES " passes, I " I

    move 0 to PASSES  move 5 to N
    perform varying I from 1 by 1 until I > N or PASSES > 8
        add 1 to PASSES
        if PASSES = 2 move GZ to G end-if
    end-perform
    display "the group moved to:  " PASSES " passes, I " I " N " N

    move 0 to PASSES  move 5 to N
    perform varying I from 1 by 1 until I > N or PASSES > 8
        add 1 to PASSES
        if PASSES = 2 initialize G  move 3 to N end-if
    end-perform
    display "initialized:         " PASSES " passes, I " I " N " N

    move 0 to PASSES
    perform varying I from 1 by 1 until I > 5 or PASSES > 8
        add 1 to PASSES
        if PASSES = 2 move x"04" to G(2:1) end-if
    end-perform
    display "a byte of the group: " PASSES " passes, I " I

    move 0 to PASSES
    perform varying I from 1 by 1 until I > 5 or PASSES > 8
        add 1 to PASSES
        if PASSES = 2 move x"01" to G(1:1) end-if
    end-perform
    display "its first byte:      " PASSES " passes, I " I

    move 0 to PASSES  move 5 to N
    perform varying I from 1 by 1 until I > N or PASSES > 8
        add 1 to PASSES
        compute K = PASSES
        if PASSES < 3 move 3 to T(K) end-if
    end-perform
    display "a table over them:   " PASSES " passes, I " I " N " N

    move 0 to PASSES
    perform varying I from 1 by 1 until I > 5 or PASSES > 8
        add 1 to PASSES
        if PASSES = 2 perform bump end-if
    end-perform
    display "a paragraph:         " PASSES " passes, I " I

    open input f1
    move 0 to PASSES  move 5 to N
    perform varying I from 1 by 1 until I > N or PASSES > 8
        add 1 to PASSES
        if PASSES = 2 read f1 into G end-if
    end-perform
    display "READ INTO:           " PASSES " passes, I " I " N " N

    move 0 to PASSES  move 9 to f1-n
    perform varying I from 1 by 1 until I > f1-n or PASSES > 8
        add 1 to PASSES
        if PASSES = 2 read f1 end-if
    end-perform
    display "the record area:     " PASSES " passes, I " I " bound " f1-n
    close f1

    open output f2
    move "zz" to st  move 0 to PASSES  move 0 to SUM1
    perform varying K from 1 by 1 until K > 3
        if ST-N = 12336 add 1 to SUM1 end-if
        move "abcd" to f2-rec  write f2-rec
        if ST-N = 12336 add 10 to SUM1 end-if
    end-perform
    display "the status, a number: " SUM1 " " ST-N
    close f2

    move 0 to PASSES
    perform varying I from 1 by 1 until I > 5 or PASSES > 8
        add 1 to PASSES
        if PASSES = 2 compute I rounded = 7 / 2 end-if
    end-perform
    display "ROUNDED:             " PASSES " passes, I " I

    move 0 to PASSES  move spaces to TXT
    perform varying P from 1 by 1 until P > 9 or PASSES > 8
        add 1 to PASSES
        string "ab" delimited by size into TXT with pointer P
    end-perform
    display "STRING's pointer:    " PASSES " passes, P " P " [" TXT "]"

    set address of LK to address of G
    move 0 to PASSES
    perform varying I from 1 by 1 until I > 5 or PASSES > 8
        add 1 to PASSES
        if PASSES = 2 move 4 to LK end-if
    end-perform
    display "a BASED item on it:  " PASSES " passes, I " I

    set address of LKG to address of G
    move 0 to PASSES  move 5 to N
    perform varying I from 1 by 1 until I > N or PASSES > 8
        add 1 to PASSES
        if PASSES = 2 move GZ to LKG end-if
    end-perform
    display "a BASED group:       " PASSES " passes, I " I " N " N

    move 0 to PASSES
    perform varying BI from 1 by 1 until BI > 5 or PASSES > 8
        add 1 to PASSES
        compute K = 3 - PASSES
        if PASSES < 3 move X20 to BE(K) end-if
    end-perform
    display "a long element:      " PASSES " passes, BI " BI

    open output relf
    perform 5 times move "rel." to relf-rec  write relf-rec end-perform
    close relf
    open input relf
    move 0 to PASSES  move 0 to RK
    perform until RK > 2 or PASSES > 8
        add 1 to PASSES
        read relf next at end move 99 to RK end-read
    end-perform
    display "the relative key:    " PASSES " passes, RK " RK
    close relf

    open output prf
    move 0 to PASSES
    perform until linage-counter > 3 or PASSES > 8
        add 1 to PASSES
        move "line" to prf-rec  write prf-rec
    end-perform
    display "LINAGE-COUNTER:      " PASSES " passes, at " linage-counter
    close prf

    move 0 to PASSES  move 6 to N
    perform varying I from 1 by 1 until I > N
        add 1 to PASSES
        subtract 1 from N
    end-perform
    display "the bound lowered:   " PASSES " passes, I " I " N " N

    move 0 to SUM1  move 0 to BS
    perform varying K from 1 by 1 until K > 3
        add 30000 to BS
        add BS to SUM1
    end-perform
    display "a store that wraps:  " BS " " SUM1

    move 0 to SUM1  move 95 to B1
    perform varying K from 1 by 1 until K > 8
        add 1 to B1
        add B1 to SUM1
    end-perform
    display "one byte, its picture: " B1 " " SUM1

    move 0 to SUM1  move 0 to BC
    perform varying K from 1 by 1 until K > 3
        add 200 to BC
        add BC to SUM1
    end-perform
    display "one byte, past it:   " BC " " SUM1

    move 0 to SUM1
    perform varying I from 1 by 1 until I > 3
        perform varying J from 1 by 1 until J > 3
            perform varying K from 1 by 1 until K > 2
                perform varying L from 1 by 1 until L > 2
                    compute SUM1 = SUM1 + I * 1000 + J * 100 + K * 10 + L
                    if I = 2 and J = 2 and K = 1 and L = 1 move 3 to J end-if
                end-perform
            end-perform
        end-perform
    end-perform
    display "four deep:           " SUM1 " " I " " J " " K " " L
    stop run.
bump.
    add 2 to I.
