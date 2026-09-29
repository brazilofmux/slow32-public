identification division.
program-id. bitstr.
*> STRING takes items of usage display or national (2023 14.9.43.3 rule 1).
data division.
working-storage section.
01  b pic 1(8) usage bit.
01  x pic x(8).
procedure division.
    string b delimited by size into x
    stop run.
