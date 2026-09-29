*> National and boolean literal forms (2023 8.3.3.4, 8.3.3.5): a doubled
*> quotation symbol inside N"..." is one quotation symbol (rule 3), in
*> either delimiter; NX"..." gives each national character as four hex
*> digits, BX"..." each boolean nibble as a hex digit.
*> docs/conformance/national-boolean.md.  No oracle: GnuCOBOL 4 national
*> data is unfinished (it DISPLAYs UTF-16 and reads BX"a5" as 00000165).
identification division.
program-id. natlitquote.
data division.
working-storage section.
01 q1 pic n(3) value n"a""b".
01 q2 pic n(3) value n'c''d'.
01 hx pic n(2) value nx"00410042".
01 bb pic 1(8) value bx"a5".
procedure division.
    display "[" q1 "] " function length(q1)
    display "[" q2 "]"
    display "[" hx "]"
    display bb
    stop run.
