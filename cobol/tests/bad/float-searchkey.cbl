       identification division.
       program-id. float-searchkey.
      * Micro Focus forbids floating point here (docs/usage.md): a SEARCH ALL key.
       data division.
       working-storage section.
       01 f comp-2.
       01 f1 comp-1.
       01 d pic 9(3).
       01 r pic 9(3).
       01 tb.
          05 e occurs 5 ascending key k indexed by xx.
             10 k comp-2.
       procedure division.
           search all e when k(xx) = 1 display "y" end-search.
           stop run.
