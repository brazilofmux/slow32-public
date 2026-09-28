identification division.
program-id. natinsp.
*> INSPECT, STRING, UNSTRING and ACCEPT of national items come later;
*> until then they are refused rather than treated as bytes.
data division.
working-storage section.
01  n pic n(4).
01  c pic 99.
procedure division.
    inspect n tallying c for all "a"
    stop run.
