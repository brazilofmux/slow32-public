*> Constant entries, the cases GnuCOBOL 4.0-early-dev cannot be asked
*> (docs/oracles.md): a constant-name defined twice the same way (2023
*> 13.10.3 rule 9 allows it; the oracle dies with SIGSEGV), BYTE-LENGTH
*> and LENGTH OF a table element named without subscripts (one element:
*> every occurrence has the one size, which is why rule 3 lets only
*> literals subscript it), LENGTH OF a national item (characters) and
*> its BYTE-LENGTH (bytes), and a GLOBAL LENGTH OF constant reaching a
*> contained program's PICTURE, filled once the containing program's
*> DATA DIVISION is laid out.  No oracle.
identification division.
program-id. constdup.
data division.
working-storage section.
01 slots      constant as 4.
01 slots      constant as 4.
01 tbl.
   05 elem    pic x(6) occurs slots.
01 nat        pic n(3).
01 elem-len   constant as length of elem.
01 elem-bytes constant as byte-length of elem in tbl.
01 nat-len    constant as length of nat.
01 nat-bytes  constant as byte-length of nat.
01 tbl-len    constant is global as length of tbl.
procedure division.
    display "slots " slots " elem " elem-len " " elem-bytes
    display "nat " nat-len " " nat-bytes " tbl " tbl-len
    call "inner"
    stop run.

identification division.
program-id. inner.
data division.
working-storage section.
01 copy-of    pic x(tbl-len) value all "*".
procedure division.
    display "inner [" copy-of "] " function length(copy-of)
    goback.
end program inner.
end program constdup.
