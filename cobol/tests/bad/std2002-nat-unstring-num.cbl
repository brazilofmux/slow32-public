identification division.
program-id. natuns.
*> A numeric UNSTRING receiver of national data must be USAGE NATIONAL
*> (2023 14.9.48.3 rule 4); a display one is
*> refused.
data division.
working-storage section.
01  s pic n(4) value n"12,3".
01  k pic 99.
procedure division.
    unstring s delimited by n"," into k
    stop run.
