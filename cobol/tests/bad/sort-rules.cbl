       identification division.
       program-id. sortrul.
      * SORT and MERGE (X3.23-1985 SORT syntax rules 1, 3, 4, 8, 10;
      * MERGE rule 7): no key in a table, USING records no longer than
      * the sort record, GIVING records no shorter, an indexed GIVING
      * file's key first and ascending, no file named twice in a MERGE,
      * and no SORT inside a SORT's procedures.
       environment division.
       input-output section.
       file-control.
           select sf assign to "s.tmp".
           select fi assign to "i.dat".
           select fo assign to "o.dat".
           select fb assign to "b.dat".
           select fs assign to "t.dat".
           select fx assign to "x.dat" organization indexed
               record key xk.
       data division.
       file section.
       sd sf.
       01 sr.
          05 sk pic x(4).
          05 st pic x occurs 2.
          05 sz pic x(14).
       fd fi.
       01 ir pic x(20).
       fd fo.
       01 orr pic x(20).
       fd fb.
       01 br pic x(40).
       fd fs.
       01 sr2 pic x(5).
       fd fx.
       01 xr.
          05 xd pic x(4).
          05 xk pic x(4).
          05 xz pic x(12).
       procedure division.
       p0.
           sort sf on ascending key st using fi giving fo.
           sort sf on ascending key sk using fb giving fo.
           sort sf on ascending key sk using fi giving fs.
           sort sf on ascending key sk using fi giving fx.
           merge sf on ascending key sk using fi fi giving fo.
           sort sf on ascending key sk input procedure p1 giving fo.
           stop run.
       p1.
           sort sf on ascending key sk using fi giving fo.
