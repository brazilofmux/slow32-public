*> Clause 8's references, swept for docs/conformance/identifiers.md (queue
*> item 19a): figurative constants (8.3.3.6: ALL literal repeated and cut
*> to the item, one character where no length is given, ALL of a symbolic
*> character, ALL ZERO to a numeric item, a figurative in a concatenation
*> expression); alphanumeric literals (8.3.3.2: doubled quotes, both
*> delimiters, hexadecimal); qualification (8.4.2.2: OF and IN, a
*> condition-name by its variable and up, an index-name by its table, a
*> paragraph by its section, from outside the sections and inside one,
*> REDEFINES unique within its group);
*> subscripts (8.4.2.3: an index-name plus or minus an integer, an
*> arithmetic expression, a condition-name subscripted, an integer-valued
*> expression that is not an integer item); NULL and ADDRESS OF
*> (8.4.3.10-11); LINAGE-COUNTER qualified (8.4.3.14, two LINAGE files);
*> PAGE-COUNTER set by the program and LINE-COUNTER read (8.4.3.15).
*> GnuCOBOL agrees.
identification division.
program-id. identifiers.
environment division.
configuration section.
special-names.
    symbolic characters dash is 46.
input-output section.
file-control.
    select pa assign to "tmp/id-a.txt" organization line sequential.
    select pb assign to "tmp/id-b.txt" organization line sequential.
    select pr assign to "tmp/id-r.txt" organization line sequential.
data division.
file section.
fd pa linage 10 lines.
01 la pic x(10).
fd pb linage 20 lines.
01 lb pic x(10).
fd pr report is r1.
working-storage section.
01 x pic x(5).
01 n pic 9(3).
01 g1.
   05 a pic x value "1".
   05 b.
      10 c pic x value "2".
   05 y pic x(3) value "def".
   05 z redefines y pic x(3).
01 g2.
   05 a pic x value "3".
   05 b.
      10 c pic x value "4".
01 t.
   05 e occurs 5 indexed by ix.
      10 v pic 9.
      10 w pic 9.
         88 w-odd value 1 3 5 7 9.
01 i pic 9 value 2.
01 p usage pointer.
01 ctr pic 99.
report section.
rd r1 page limit 10 lines.
01 d1 type detail.
   05 line plus 1 column 1 pic 99 source page-counter.
   05 column 5 pic 99 source line-counter.
procedure division.
    move all "ab" to x display "[" x "]"
    move all dash to x display "[" x "]"
    move all zero to n display "[" n "]"
    display "[" space "]" "[" all "xy" "]" "[" "a" & quote & "b" "]"
    move "a""b'c" to x display "[" x "]"
    move 'a''b"c' to x display "[" x "]"
    move x"414243" to x display "[" x "]"
    display a of g1 a in g2 c of b of g1 c of g2 c in b in g2 z of g1
    perform varying ix from 1 by 1 until ix > 5
        set v (ix) to ix
        move v (ix) to w (ix)
    end-perform
    set ix to 2
    display v (ix + 1) v (ix - 1) v (ix) v (i * 2) v (i + 1 - 1) v (6 / 2)
    if w-odd of w of e of t (ix + 1) display "odd at 3" end-if
    if w-odd (ix) display "odd at 2" else display "even at 2" end-if
    set p to null
    if p = null display "null" end-if
    set p to address of x
    if p not = null and p = address of x display "address" end-if
    go to p1 of s1.
s1 section.
p1.
    display "p1 of s1"
    go to p1 of s2.
s2 section.
p1.
    display "p1 of s2"
    go to p2.
p2.
    open output pa pb pr
    write la from "a" write la from "a" write la from "a"
    write lb from "b"
    move linage-counter of pa to ctr display "lc a " ctr
    move linage-counter in pb to ctr display "lc b " ctr
    initiate r1
    generate d1
    move 7 to page-counter
    generate d1
    add 1 to page-counter of r1
    generate d1
    move line-counter to ctr display "line-counter " ctr
    terminate r1
    close pa pb pr
    stop run.
