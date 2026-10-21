identification division.
program-id. lockseq.
*> Sequential lock identity is the record's start, so a WRITE in EXTEND
*> at EOF is not the last record an AUTOMATIC READ just locked.  Line
*> sequential too.  No oracle: GnuCOBOL's locks are between processes,
*> none within one run unit.  No gcobol.
environment division.
input-output section.
file-control.
    select qa assign to "lkseq.dat" organization sequential file status qsa
        lock mode is automatic.
    select qb assign to "lkseq.dat" organization sequential file status qsb
        lock mode is automatic.
    select la assign to "lkseq.txt" organization line sequential file status lsa
        lock mode is automatic.
    select lb assign to "lkseq.txt" organization line sequential file status lsb
        lock mode is automatic.
data division.
file section.
fd qa.
01 rqa pic x(4).
fd qb.
01 rqb pic x(4).
fd la.
01 rla pic x(8).
fd lb.
01 rlb pic x(8).
working-storage section.
01 qsa pic xx.
01 qsb pic xx.
01 lsa pic xx.
01 lsb pic xx.
procedure division.
    open output qa
    move "aaaa" to rqa write rqa
    move "bbbb" to rqa write rqa
    close qa
    open input qa
    read qa
    read qa
    display "qa holds last sequential: " qsa " " rqa
    open extend qb
    move "cccc" to rqb
    write rqb
    display "qb EXTEND write at EOF: " qsb
    close qa close qb
    open output la
    move "one" to rla write rla
    move "two" to rla write rla
    close la
    open input la
    read la
    read la
    display "la holds last line: " lsa " " rla
    open extend lb
    move "three" to rlb
    write rlb
    display "lb EXTEND write at EOF: " lsb
    close la close lb
    stop run.
