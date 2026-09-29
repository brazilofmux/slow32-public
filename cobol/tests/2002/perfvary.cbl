identification division.
program-id. perfvary.
*> PERFORM VARYING an index-name FROM an identifier (2023 14.9.28.4 rule
*> 3): the identifier's value must be positive; with checking on, one
*> that is not is EC-RANGE-PERFORM-VARYING, a fatal condition -- the USE
*> declarative runs and the run ends.  A positive value varies as usual.
*> No oracle: GnuCOBOL 4 does not implement exception declaratives.
data division.
working-storage section.
01  t.
    05 e pic x occurs 5 indexed by ix.
01  k    pic s9 value 2.
procedure division.
declaratives.
ur section.
    use after exception condition ec-range-perform-varying.
ur1.
    display "  EC-RANGE-PERFORM-VARYING".
end declaratives.
main section.
m1.
>>TURN EC-RANGE-PERFORM-VARYING CHECKING ON
    move "abcde" to t
    perform varying ix from k by 1 until ix > 4
        display "e(ix) = " e(ix)
    end-perform
    move 0 to k
    perform varying ix from k by 1 until ix > 4
        display "not expected"
    end-perform
    display "after (not expected)"
    stop run.
