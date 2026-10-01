*> Concatenation with a figurative constant and with national literals
*> (2002 and 2023 8.8.3): rule 1 lets either operand, or both, be a
*> figurative constant, of the other operand's class (general rule 1a;
*> both, alphanumeric, 1b), and joins national with national.
*> No oracle: GnuCOBOL 4.0-early-dev refuses ZERO & "1" ("only literals
*> with the same category can be concatenated") and has no DISPLAY-OF
*> (docs/oracles.md).
identification division.
program-id. concatfig.
data division.
working-storage section.
01 w        pic x(12).
01 n        pic n(6) value n"ab" & n"cd" & space.
01 b        pic 1(4) value b"10" & zero & b"1".
procedure division.
    move zero & "1" & zero to w
    display "[" w "]"
    move space & quote to w
    display "[" w "]"
    display function length(n) " [" function display-of(n) "]"
    display b
    stop run.
