*> The ALL subscript of the 1989 amendment: every element of a table
*> as one argument of a numeric function. GnuCOBOL 4 refuses the form
*> altogether (unexpected ALL), so the oracle's refusal is the
*> documented state of the art; the values are checked against the
*> same sums taken element by element in free/intrinsics.
identification division.
program-id. fnall.
data division.
working-storage section.
77  r pic s9(9)v9(4).
01  t.
    05  e occurs 4 pic 99.
procedure division.
    move 10 to e (1). move 30 to e (2). move 20 to e (3). move 40 to e (4).
    compute r = function sum(e (all)) display 'sum      ' r.
    compute r = function mean(e (all)) display 'mean     ' r.
    compute r = function range(e (all)) display 'range    ' r.
    compute r = function median(e (all)) display 'median   ' r.
    stop run.
end program fnall.
