*> -std=2023, the first of it (docs/plans/standard-queue.md item 30): the
*> OPTIONS INITIALIZE clause (2023 11.9.10) -- every item of the named
*> sections without a VALUE starts as the fill byte, a VALUE kept; the
*> fill BINARY ZEROES, SPACES, HIGH-VALUES, LOW-VALUES or a one-byte
*> X"..".  And SYNCHRONIZED on a group (13.18.55.4 rule 1): as if on each
*> elementary item below it.  A contained program has its own clause (a
*> numeric item filled with spaces holds no number: shown through a
*> redefinition).
*> No oracle: GnuCOBOL 4 has neither.  docs/conformance/edition-2023.md
identification division.
program-id. optinit.
options.
    initialize working-storage section to x"2a".
data division.
working-storage section.
01 g.
   05 a pic x(3).
   05 b pic 9(3).
   05 c pic x(2) value "ok".
   05 d pic s9(4) comp.
   05 e pic 9 value 7.
01 f pic x(2).
01 sg sync.
   05 s1 pic x.
   05 s2 pic s9(4) comp.
   05 s3 pic x.
   05 s4 pic s9(8) comp value 12.
01 sh.
   05 t1 pic x.
   05 t2 pic s9(4) comp.
   05 t3 pic x.
   05 t4 pic s9(8) comp value 12.
procedure division.
    display "[" g "] [" f "]"
    display "sync group " function length(sg) ", plain group " function length(sh) " " s4 " " t4
    call "optinit-sub"
    stop run.
identification division.
program-id. optinit-sub.
options.
    initialize all section to spaces.
data division.
working-storage section.
01 n pic 9(3).
01 nx redefines n pic x(3).
01 w pic x(3).
01 v pic x(3) value "val".
local-storage section.
01 l pic x(2).
procedure division.
    display "sub [" nx "][" w "][" v "][" l "]"
    goback.
end program optinit-sub.
end program optinit.
