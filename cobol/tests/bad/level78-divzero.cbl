identification division.
program-id. l78div.
*> Micro Focus's integer arithmetic: a division by zero is refused.
data division.
working-storage section.
78 bad value 4 / (2 - 2).
procedure division.
    stop run.
