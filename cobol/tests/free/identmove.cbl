*> identmove -- a MOVE between items with identical descriptors is a byte
*> copy, not a decode and a re-encode.  Every sending item here holds bytes
*> cob_put_num would never write; the standard says nothing about what a
*> numeric item holding non-digits means, and GnuCOBOL (measured) passes
*> them through.  Before GitHub #27 s32-cobc normalised all four: the
*> unsigned DISPLAY arrived as 0120451230, the overpunch as 0000000133, the
*> COMP-3 lost its 0xF sign nibble, and the COMP was truncated to its four
*> digits.  The two binary cases are checked by comparing the destination
*> bytes with the source bytes, so the test does not depend on either
*> system's COMP byte order.  COMP-3: default dialect (-std=cobol85
*> rejects the usage, so the oracle never sees this program without it).
identification division.
program-id. identmove.
data division.
working-storage section.
01  raw           pic x(10) value ' 12 45abc '.
01  src  redefines raw pic 9(10).
01  dst           pic 9(10) value 0.
01  dstx redefines dst pic x(10).

01  nraw          pic x(10) value '000000012}'.
01  nsrc redefines nraw pic s9(10).
01  ndst          pic s9(10) value 0.
01  ndstx redefines ndst pic x(10).

*> a signed COMP-3 carrying the 0xF sign a foreign system writes
01  praw          pic x(6)  value x'00000000012F'.
01  psrc redefines praw pic s9(11) comp-3.
01  pdst          pic s9(11) comp-3 value 0.
01  pdstx redefines pdst pic x(6).

*> a four-digit COMP holding far more than four digits' worth
01  braw          pic x(2)  value x'60EA'.
01  bsrc redefines braw pic 9(4) comp.
01  bdst          pic 9(4) comp value 0.
01  bdstx redefines bdst pic x(2).

procedure division.
main-procedure.
    move src to dst
    display 'unsigned  [' dstx ']'
    move nsrc to ndst
    display 'overpunch [' ndstx ']'
    move psrc to pdst
    if pdstx = praw
        display 'packed    verbatim'
    else
        display 'packed    REWRITTEN'
    end-if
    move bsrc to bdst
    if bdstx = braw
        display 'binary    verbatim'
    else
        display 'binary    REWRITTEN'
    end-if
    stop run.
end program identmove.
