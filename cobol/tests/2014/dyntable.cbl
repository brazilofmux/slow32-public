identification division.
program-id. dyntable.
*> Dynamic-capacity tables (COBOL 2014; 2023 13.18.38 format 4, 8.5.1.9,
*> 14.9.39 format 14): OCCURS DYNAMIC with CAPACITY IN, FROM, TO and
*> INITIALIZED.  A store past the capacity makes the elements up to it
*> (8.5.1.9.3), EC-BOUND-OVERFLOW the first time the expected capacity is
*> passed; a read past it is EC-BOUND-SUBSCRIPT, as for a fixed table of
*> that many; SET capacity TO / UP BY / DOWN BY, clamped to the minimum,
*> EC-BOUND-SET past the expected capacity; the capacity item a sending
*> operand; INITIALIZE of the group reaches every element and leaves the
*> capacity; SEARCH, SEARCH ALL, SORT and the ALL subscript run to the
*> capacity; items after the table keep their place; a VALUE among the
*> elements makes the initial capacity the expected one (13.18.63.4 rule
*> 16b).  No oracle: GnuCOBOL 4 has no OCCURS DYNAMIC.  No gcobol either.
data division.
working-storage section.
01 g.
   05 head-x pic x(3) value "aaa".
   05 t occurs dynamic capacity in cap to 5 initialized
        ascending key is k indexed by ix.
      10 k pic 9(3) value 7.
      10 v pic x(4) value "init".
   05 tail-x pic x(3) value "zzz".
01 u.
   05 e pic x(2) occurs dynamic capacity in ecap from 2.
01 m.
   05 q pic 9(2) occurs dynamic capacity in qcap descending key is q indexed by qx.
01 n pic 9(4) value 0.
01 i pic 9(4).
01 w pic x(4).
01 r pic 9(3).
procedure division.
declaratives.
d1 section. use after exception condition ec-bound-subscript.
p1. display "  declarative: " function trim(function exception-status).
d2 section. use after exception condition ec-bound-overflow.
p2. display "  nonfatal: " function trim(function exception-status) " cap=" cap.
d3 section. use after exception condition ec-bound-set.
p3. display "  nonfatal: " function trim(function exception-status) " cap=" cap.
end declaratives.
main section.
m1.
    display "start: cap=" cap " ecap=" ecap " qcap=" qcap " head=" head-x " tail=" tail-x.
    move 11 to k(1). move "one " to v(1).
    display "store 1: cap=" cap " k(1)=" k(1) " v(1)=" v(1).
    move 33 to k(3).
    display "store 3: k(2)=" k(2) " v(2)=" v(2) " k(3)=" k(3) " v(3)=" v(3).
    display "neighbours intact: " head-x "/" tail-x.
    move 2 to i. move "two " to v(i).
    display "v(i): " v(2).
>>TURN EC-BOUND-SUBSCRIPT EC-BOUND-OVERFLOW EC-BOUND-SET CHECKING ON
    move 55 to k(5).
    display "store 5: cap=" cap " k(4)=" k(4) " k(5)=" k(5).
    move 66 to k(6).
    display "store 6 past the expected 5: cap=" cap " k(6)=" k(6).
    move 77 to k(7).
    display "store 7, already past: cap=" cap.
    set cap to 3.
    display "set cap to 3: cap=" cap " k(3)=" k(3).
    set cap up by 2.
    display "set cap up by 2: cap=" cap " k(5)=" k(5) " (made again, initialized)".
    set cap down by 10.
    display "set cap down by 10: cap=" cap " (the minimum)".
    move 9 to i. set cap to i.
    display "set cap to i=9: cap=" cap.
    move 4 to i. set cap to i * 2.
    display "set cap to i * 2: cap=" cap.
    display "u: ecap=" ecap " e(1)=[" e(1) "] e(2)=[" e(2) "] (the minimum, defaults)".
    move "ab" to e(1). move "cd" to e(2). move "ef" to e(3).
    display "u stored: ecap=" ecap " " e(1) e(2) e(3).
    set ecap down by 5.
    display "down to the minimum: ecap=" ecap.
    initialize g.
    display "initialize g: cap=" cap " k(1)=" k(1) " v(1)=[" v(1) "] head=[" head-x "]".
    initialize g with filler all to value then to default.
    display "all to value: k(1)=" k(1) " v(1)=[" v(1) "] head=[" head-x "]".
    perform varying i from 1 by 1 until i > cap
        move i to k(i)
        move function char(i + 65) to v(i)
    end-perform.
    display "k(1..8): " k(1) k(2) k(3) k(4) k(5) k(6) k(7) k(8).
    display "max k: " function max(k(all)) " sum k: " function sum(k(all)).
    set ix to 1.
    search t at end display "serial: not found"
        when k(ix) = 6 set i to ix display "serial: found k=6 at " i " v=" v(ix)
    end-search.
    set ix to 1.
    search t at end display "serial: 99 not found, cap=" cap
        when k(ix) = 99 display "serial: found 99?"
    end-search.
    search all t at end display "binary: not found"
        when k(ix) = 7 set i to ix display "binary: found 7 at " i
    end-search.
    search all t at end display "binary: 9 not found (past the capacity)"
        when k(ix) = 9 display "binary: found 9?"
    end-search.
    move 5 to q(1). move 9 to q(2). move 1 to q(3). move 7 to q(4).
    sort q.
    display "sort q descending: " q(1) q(2) q(3) q(4) " qcap=" qcap.
    sort q on ascending key q.
    display "sort q ascending: " q(1) q(2) q(3) q(4).
    display "end: " head-x tail-x.
    move k(9) to r.
    display "not reached: " r.
    stop run.
