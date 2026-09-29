identification division.
program-id. redefp.
*> A pointer item is neither redefined nor a redefinition (2023
*> 13.18.44.3 rules 12 and 14).
data division.
working-storage section.
01 p usage pointer.
01 q redefines p pic x(4).
procedure division.
    stop run.
