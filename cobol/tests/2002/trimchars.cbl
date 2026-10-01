*> FUNCTION TRIM's characters to delete (2023 15.96, argument-2), each
*> taken completely in turn (returned value rule 5), and a national
*> argument trimmed of national spaces.  No oracle: GnuCOBOL
*> 4.0-early-dev has TRIM's 2014 form only (docs/oracles.md).
identification division.
program-id. trimchars.
data division.
working-storage section.
01 s   pic x(12) value "**--ab-c*--*".
01 z   pic x(8)  value "00012300".
01 n   pic n(6)  value n"  xy  ".
procedure division.
    display "[" function trim(s "*") "]"
    display "[" function trim(s "*" "-") "]"
    display "[" function trim(s "-" "*") "]"
    display "[" function trim(z leading "0") "]"
    display "[" function trim(z trailing "0") "]"
    display "[" function display-of(function trim(n)) "] " function length(function trim(n))
    display "[" function trim(z "0" "1" "2" "3") "]" function length(function trim(z "0" "1" "2" "3"))
    stop run.
