identification division.
program-id. p-std2023-baseconvert-same.
*> BASECONVERT: the bases differ (15.12.3 rule 1).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
01 sn pic s9(3) value -1.
01 fl usage float-long.
01 r pic x(20).

procedure division.
    move function baseconvert("12" 10 10) to r.
    stop run.
