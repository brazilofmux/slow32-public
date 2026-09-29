*> SEARCH ALL as a binary search (2023 14.9.37): one ascending key; a
*> descending key then an ascending one, both in the WHEN, joined by AND;
*> a table whose bound is an OCCURS DEPENDING ON item, keys past it not
*> found.  docs/conformance/usage.md (SEARCH ALL keys)
identification division.
program-id. searchall.
data division.
working-storage section.
01 t1.
   05 e1 occurs 10 ascending key is k1 indexed by x1.
      10 k1 pic 99.
      10 v1 pic x.
01 t2.
   05 e2 occurs 8 descending key is k2a ascending key is k2b indexed by x2.
      10 k2a pic 9.
      10 k2b pic 9.
      10 v2 pic x.
01 cnt pic 99 value 7.
01 t3.
   05 e3 occurs 1 to 9 depending on cnt ascending key k3 indexed by x3.
      10 k3 pic 99.
01 w pic 99.
01 a pic 9.
01 b pic 9.
01 i pic 99.
procedure division.
    perform varying i from 1 by 1 until i > 10
        compute k1(i) = i * 3
        move function char(65 + i) to v1(i)
    end-perform
    move "31a32b33c21d22e11f12g13h" to t2
    perform varying i from 1 by 1 until i > 9
        compute k3(i) = i * 2
    end-perform
    perform varying w from 0 by 1 until w > 31
        search all e1 at end continue
            when k1(x1) = w display "k1 " w " " v1(x1)
        end-search
    end-perform
    perform varying a from 0 by 1 until a > 3
        perform varying b from 0 by 1 until b > 3
            search all e2 at end continue
                when k2a(x2) = a and k2b(x2) = b display "k2 " a b " " v2(x2)
            end-search
        end-perform
    end-perform
    perform varying w from 0 by 1 until w > 20
        search all e3 at end display "k3 " w " not found"
            when k3(x3) = w display "k3 " w " found"
        end-search
    end-perform
    stop run.
