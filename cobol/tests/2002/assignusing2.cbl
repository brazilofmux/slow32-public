*> ASSIGN TO literal USING data-name (2023 12.4.5): the literal names the
*> file while the item holds spaces, the item's content once it holds a
*> name -- the implementor's consistency rule between the two (GR 3b, 4),
*> chosen so that a program may carry a default name in the entry and
*> override it at run time.  No oracle: GnuCOBOL 4 does not take the
*> two together.
*> docs/conformance/files.md
identification division.
program-id. assignusing2.
environment division.
input-output section.
file-control.
    select f2 assign to "tmp/au-lit.txt" using fname2
        organization line sequential file status fs.
data division.
file section.
fd f2.
01 r2 pic x(20).
working-storage section.
01 fs pic xx.
01 fname2 pic x(30) value spaces.
procedure division.
    open output f2 write r2 from "by the literal" close f2
    move "tmp/au-item.txt" to fname2
    open output f2 write r2 from "by the item" close f2
    open input f2 read f2 display "item    " r2 close f2
    move spaces to fname2
    open input f2 read f2 display "literal " r2 close f2
    stop run.
