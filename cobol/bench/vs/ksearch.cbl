*> ksearch -- SEARCH ALL over a 2,000-entry ascending table and serial
*> SEARCH over a 64-entry one, keys computed from the loop index.
identification division.
program-id. ksearch.
data division.
working-storage section.
01  n        pic 9(9) comp value 1000000.
01  i        pic 9(9) comp.
01  k        pic 9(9) comp.
01  big.
    05  be   occurs 2000 ascending key is bk indexed by bx.
        10  bk   pic 9(8) comp.
        10  bv   pic 9(4) comp.
01  small.
    05  se   occurs 64 indexed by sx.
        10  sk   pic x(3).
        10  sv   pic 9(4) comp.
01  want     pic x(3).
01  d1       pic 9.
01  found    pic 9(9) comp value 0.
01  tot      pic 9(15) comp-3 value 0.
procedure division.
    perform varying k from 1 by 1 until k > 2000
        compute bk(k) = k * 7
        compute bv(k) = function mod(k, 1000)
    end-perform
    perform varying k from 1 by 1 until k > 64
        move k to sv(k)
        move function mod(k, 10) to d1
        move d1 to sk(k)(1:1)
        move function char(65 + function mod(k, 26)) to sk(k)(2:1)
        move function char(66 + function mod(k * 3, 20)) to sk(k)(3:1)
    end-perform
    perform varying i from 1 by 1 until i > n
        compute k = function mod(i * 13, 14007)
        search all be
            at end continue
            when bk(bx) = k
                add 1 to found
                add bv(bx) to tot
        end-search
        compute k = function mod(i, 64) + 1
        move sk(k) to want
        set sx to 1
        search se
            at end continue
            when sk(sx) = want add sv(sx) to tot
        end-search
    end-perform
    display "ksearch " found " " tot
    stop run.
