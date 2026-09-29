identification division.
program-id. bvg.
*> Without GROUP-USAGE BIT a group of bit items is an alphanumeric group
*> (2023 13.18.29.4 rule 3); a boolean VALUE on it would store the
*> literal's characters, not its bits. Refused.
data division.
working-storage section.
01  grp value b"1100101".
    05 g1    pic 1(3) usage bit.
    05 g2    pic 1(4) usage bit.
procedure division.
    display g1
    stop run.
