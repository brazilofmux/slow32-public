identification division.
program-id. bep.
*> Reference modification of a bit array's element counts the element's
*> bits (2023 8.4.3.3.4 rule 5a): (3:2) runs past a 3-bit element even
*> though the array goes on.
data division.
working-storage section.
01  tri.
    05 pr    pic 1(3) usage bit occurs 5.
procedure division.
    display pr(1)(3:2)
    stop run.
