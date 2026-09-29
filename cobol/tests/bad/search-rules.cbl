       identification division.
       program-id. srchrul.
      * SEARCH (X3.23-1985 SEARCH syntax rules 1, 4 and 5): SEARCH ALL
      * needs a KEY; its one WHEN tests keys for equality, AND only, each
      * subscripted by the first index, the value no key and not indexed
      * by it, the keys a leading run; NEXT SENTENCE with no END-SEARCH.
       data division.
       working-storage section.
       01 t.
          05 e occurs 5 ascending key k1 k2 indexed by i j.
             10 k1 pic x(2).
             10 k2 pic 9.
             10 v pic x.
       01 u.
          05 f pic x occurs 5 indexed by fi.
       01 w pic x(2).
       01 n pic 9.
       procedure division.
           search all f when f (fi) = "a" continue end-search.
           search all e when v (i) = "a" continue end-search.
           search all e when k2 (i) = n continue end-search.
           search all e when k1 (j) = w continue end-search.
           search all e when k1 (i) = w or k2 (i) = n continue
               end-search.
           search all e when k1 (i) > w continue end-search.
           search all e when k1 (i) = v (i) continue end-search.
           search all e when w = k1 (i) continue end-search.
           search all e when k1 (i) = w continue
               when k1 (i) = "zz" continue end-search.
           search e when v (i) = "a" next sentence end-search.
           stop run.
