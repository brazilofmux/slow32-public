*> BP-E20 under -std=2002: a national literal of 161 positions (2002
*> 8.3.1.2.4.2 rule 1 allows 160; 2014 and 2023 allow 8,191); taken.
identification division.
program-id. p.
data division.
working-storage section.
01 i pic n(200) value n"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".
procedure division.
    stop run.
