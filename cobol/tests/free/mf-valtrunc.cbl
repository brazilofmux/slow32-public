*> Micro Focus's dialect (-dialect=mf; BP-D7): a VALUE literal longer than
*> its alphanumeric item is cut on the right to the item -- JUSTIFIED does
*> not change that, initialization not being affected by it (2023
*> 13.18.63.4 rule 7) -- and the compiler says so, always, with a warning.  abrignoli_COBSOFT's copybook has a
*> 19-character mask in a PIC X(18).  The user's ruling (2026-10-01): MF's
*> reference forbids it as the standard does, so it is taken only with
*> the warning.
identification division.
program-id. mf-valtrunc.
data division.
working-storage section.
01 mask   pic x(18) value "  .   .   /    -   ".
01 right8 pic x(8) justified right value "0123456789".
01 fits   pic x(6) value "abc".
procedure division.
    display "[" mask "]"
    display "[" right8 "]"
    display "[" fits "]"
    stop run.
