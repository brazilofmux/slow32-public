*> ALLOCATE data-name INITIALIZED (2023 14.9.3.4 GR 7): the new storage
*> set as INITIALIZE data-name WITH FILLER ALL TO VALUE THEN TO DEFAULT
*> -- VALUE clauses, FILLER included, the rest to their category's
*> default.  Without INITIALIZED the storage comes zeroed here.
*> docs/conformance/initialize.md
identification division.
program-id. allocinit.
data division.
working-storage section.
01 p usage pointer.
01 r based.
   05 a  pic x(3) value "abc".
   05 filler pic x(2) value "zz".
   05 n  pic 9(3) value 7.
   05 e  pic zz9.
   05 m  pic 9(2).
   05 t  pic x(2).
procedure division.
    allocate r initialized returning p
    display "[" r "]"
    move "!!!!!!!!!!" to r (1:10)
    free p
    allocate r initialized
    display "[" r "]"
    stop run.
