identification division.
program-id. dynlen.
*> Dynamic-length elementary items (COBOL 2014; 2023 13.18.19, 8.5.1.10,
*> 14.9.39 format 16, 12.3.7 DYNAMIC LENGTH STRUCTURE): an alphanumeric or
*> national item whose length is its content's.  A MOVE to it sets content
*> and length (a figurative constant one character, ALL literal the
*> literal, a zero-length literal nothing; the limit cuts on the right); it
*> sends, compares and is reference-modified at its current length, and
*> FUNCTION LENGTH / BYTE-LENGTH count it; SET SIZE OF pads with spaces or
*> cuts; INITIALIZE makes it empty; a VALUE is its initial content; under
*> OCCURS each occurrence has its own length; items beside it keep their
*> place.  INSPECT REPLACING and CONVERTING work in place at the current
*> length; STRING INTO grows the item under its pointer, UNSTRING INTO and
*> ACCEPT make the examined characters or the line its content.  No oracle: GnuCOBOL 4 has no DYNAMIC LENGTH.  No gcobol either.
environment division.
configuration section.
special-names.
    dynamic length structure dls-pfx is prefixed
    dynamic length structure dls-short is short prefixed.
data division.
working-storage section.
01 s pic x dynamic length.
01 t pic x dynamic length dls-pfx limit is 10 value "hello".
01 u pic x dynamic length dls-short.
01 n pic n dynamic length limit 20.
01 g.
   05 a pic x(3) value "abc".
   05 d pic x dynamic length occurs 3.
   05 z pic x(2) value "zz".
01 i pic 9(4).
01 w pic x(20).
01 num pic 9(5) value 42.
01 sgn pic s9(3) value -7.
procedure division.
    display "start: s=[" s "] len=" function length(s) " t=[" t "] len=" function length(t) " bytes=" function byte-length(t).
    move "a longer text" to s.
    display "move: s=[" s "] len=" function length(s).
    move "x" to s.
    display "shorter: s=[" s "] len=" function length(s).
    move "this is longer than ten" to t.
    display "limit 10: t=[" t "] len=" function length(t).
    move spaces to s.
    display "spaces: s=[" s "] len=" function length(s).
    move all "ab" to s.
    display "all ab: s=[" s "] len=" function length(s).
    move "" to s.
    display "empty: s=[" s "] len=" function length(s).
    move num to s.
    display "numeric: s=[" s "] len=" function length(s).
    move sgn to s.
    display "signed: s=[" s "] len=" function length(s).
    move a to s.
    display "from item: s=[" s "] len=" function length(s).
    move s to w.
    display "to fixed: w=[" w "]".
    if s = "abc" display "compare equal" end-if.
    if s = "abc " display "compare with trailing space equal (standard padding)" end-if.
    if s not = "abd" display "compare not equal" end-if.
    move "hello world" to s.
    display "part: [" s(7:5) "] [" s(1:5) "] [" s(7:) "]".
    move 3 to i.
    display "computed part: [" s(i:3) "] [" s(i:) "]".
    move "XY" to s(1:2).
    display "part receiving: s=[" s "] len=" function length(s).
    set size of s to 5.
    display "size 5: s=[" s "] len=" function length(s).
    set size of s to 8.
    display "size 8: s=[" s "|] len=" function length(s).
    set size of s to i + 1.
    display "size i+1: s=[" s "] len=" function length(s).
    string "abc" "def" delimited by size into w.
    move function upper-case(s) to s.
    display "upper: s=[" s "]".
    move function trim(w) to s.
    display "trim(w): s=[" s "] len=" function length(s).
    display "national: n=[" n "] len=" function length(n) " bytes=" function byte-length(n).
    move "héllo" to n.
    display "national move: len=" function length(n) " bytes=" function byte-length(n).
    move function display-of(n) to s.
    display "national to alnum: s=[" s "] len=" function length(s).
    move "one" to d(1). move "two" to d(2).
    display "table: d(1)=[" d(1) "] d(2)=[" d(2) "] d(3)=[" d(3) "] lens " function length(d(1)) " " function length(d(3)) " a=" a " z=" z.
    initialize g.
    display "initialize g: d(1)=[" d(1) "] len=" function length(d(1)) " a=[" a "]".
    move "again" to d(2).
    initialize d(2).
    display "initialize d(2): len=" function length(d(2)).
    move "tally me" to s.
    move 0 to i.
    inspect s tallying i for all "l".
    display "tallying: " i.
    inspect s replacing all "l" by "L".
    display "replacing: [" s "] len=" function length(s).
    inspect s converting "aeiou" to "AEIOU".
    display "converting: [" s "]".
    move "ab" to s.
    string "cd" "efg" delimited by size into s.
    display "string into: [" s "] len=" function length(s) " (the pointer from 1: the content replaced from there, grown)".
    move 3 to i.
    string "XYZ" delimited by size into s with pointer i.
    display "string with pointer 3: [" s "] len=" function length(s) " p=" i.
    move 9 to i.
    string "!" delimited by size into s with pointer i.
    display "string with pointer 9: [" s "] len=" function length(s) " (a gap of spaces)".
    move "alpha,beta gamma" to w.
    unstring w delimited by "," or " " into s t.
    display "unstring: s=[" s "] " function length(s) " t=[" t "] " function length(t).
    accept s.
    display "accept: [" s "] len=" function length(s).
    accept t.
    display "accept empty line: [" t "] len=" function length(t).
    display "u: len=" function length(u) " [" u "]".
    stop run.
