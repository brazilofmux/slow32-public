identification division.
program-id. inspord.
*> INSPECT with several phrases: the comparison cycle goes position by
*> position, left to right; at each position the phrases are tried in the
*> order written, the first match wins, and the next cycle starts right
*> of the match (X3.23-1985 VI-96, general rule 6).  BEFORE and AFTER
*> boundaries are fixed before the first cycle (rule 7), and a LEADING
*> run begins where comparison began in the first cycle the phrase was
*> eligible for (rule 13c).  Found by the differential generator
*> (tests/gen, which checks every INSPECT against inspect85.py, these
*> rules written out); the oracle applies the phrases one after another
*> over the whole item (docs/oracles.md).
data division.
working-storage section.
01 s1 pic x(11).
01 s2 pic x(13).
01 s3 pic x(14).
01 k pic 9(4).
procedure division.
*> b at 3 becomes 1; at 4, "1b" matches the second phrase, which takes
*> positions 4 and 5: the b at 5 is not the first phrase's
    move ",Bb1baB" to s1
    inspect s1 replacing all "b" by "1" all "1b" by "1b"
        all "1" by "a" after initial "1"
    display "1 [" s1 "]"
*> the LEADING phrase is eligible from position 4 (after the first A);
*> the FIRST phrase takes the B there, so no leading run begins
    move " bABB1A1b1A1" to s2
    inspect s2 replacing first "B" by "," leading "B" by " " after initial "A"
    display "2 [" s2 "]"
*> ",b" at 4 is matched by the FIRST phrase before the ALL "b" phrase
*> reaches the b at 5
    move "1Aa,bAa" to s3
    move 0 to k
    inspect s3 tallying k for characters before initial "a"
        replacing all "b" by "A" first ",b" by "bA" all "A1" by "1a"
    display "3 [" s3 "] " k
    stop run.
