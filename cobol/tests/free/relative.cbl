identification division.
program-id. relative.
*> Relative I-O (level 1 of the module): random WRITE/READ/REWRITE/DELETE
*> by RELATIVE KEY, sequential READ NEXT that skips empty slots and tells
*> the key item the record number, START by relation, and sequential
*> access where WRITE fills the next slot and REWRITE/DELETE act on the
*> last record read.  Statuses measured against GnuCOBOL (22 occupied,
*> 23 absent, 24 boundary, 43 no prior READ).  The bytes on disk are ours
*> (docs/oracles.md), so only the program's output is compared.
environment division.
input-output section.
file-control.
    select relf assign to 'tmp/rel.dat'
        organization is relative
        access is random
        relative key is rk
        file status is st.
    select reld assign to 'tmp/rel.dat'
        organization is relative
        access is dynamic
        relative key is dk
        file status is st.
    select rels assign to 'tmp/rels.dat'
        organization is relative
        access is sequential
        relative key is sk
        file status is st.
data division.
file section.
fd  relf.
01  relf-rec         pic x(8).
fd  reld.
01  reld-rec         pic x(8).
fd  rels.
01  rels-rec         pic x(8).
working-storage section.
01  rk               pic 9(4).
01  dk               pic 9(4).
01  sk               pic 9(4) value 7.
01  st               pic xx.
01  eof              pic x value 'n'.
procedure division.
main.
    open output relf.
    display 'open ' st.
    move 1 to rk. move 'first   ' to relf-rec. write relf-rec invalid key display 'inv1' end-write. display 'w1 ' st.
    move 3 to rk. move 'third   ' to relf-rec. write relf-rec invalid key display 'inv3' end-write. display 'w3 ' st.
    move 3 to rk. move 'dup     ' to relf-rec. write relf-rec invalid key display 'dup3 ' st end-write.
    move 5 to rk. move 'fifth   ' to relf-rec. write relf-rec invalid key display 'inv5' end-write.
    move 0 to rk. write relf-rec invalid key display 'inv0 ' st end-write.
    close relf.
    open i-o relf.
    move 3 to rk. delete relf invalid key display 'del3 ' st end-delete. display 'd3 ' st.
    move 2 to rk. read relf invalid key display 'r2 ' st end-read.
    move 5 to rk. read relf invalid key display 'r5inv' end-read. display 'r5 ' st ' [' relf-rec ']'.
    move 9 to rk. read relf invalid key display 'r9 ' st end-read.
    move 5 to rk. move 'FIFTH   ' to relf-rec. rewrite relf-rec invalid key display 'rw5inv' end-rewrite. display 'rw5 ' st.
    move 4 to rk. rewrite relf-rec invalid key display 'rw4 ' st end-rewrite.
    close relf.
*>  the same file through dynamic access: READ NEXT skips slots 2-4, START positions
    open input reld.
    display 'open ' st.
    perform until eof = 'y'
        read reld next record at end move 'y' to eof
            not at end display 'next ' st ' dk=' dk ' [' reld-rec ']'
        end-read
    end-perform.
    display 'eof ' st.
    move 2 to dk.
    start reld key is not less than dk invalid key display 'start ' st end-start.
    display 'start ' st ' dk=' dk.
    read reld next record at end display 'end' not at end display 'after start ' dk ' [' reld-rec ']' end-read.
    move 3 to dk.
    start reld key is greater than dk invalid key display 'start2 ' st end-start.
    read reld next record at end display 'end' not at end display 'after start2 ' dk ' [' reld-rec ']' end-read.
    move 6 to dk.
    start reld key is greater than dk invalid key display 'start3 ' st end-start.
    move 5 to dk.
    start reld key is equal to dk invalid key display 'start4 ' st end-start.
    read reld next record at end display 'end' not at end display 'after start4 ' dk ' [' reld-rec ']' end-read.
    move 1 to dk.
    read reld invalid key display 'r1inv' end-read. display 'random through dynamic: ' st ' [' reld-rec ']'.
    close reld.
*>  sequential access: WRITE fills the next slot and sets the key item
    open output rels.
    move 'one     ' to rels-rec. write rels-rec. display 'w ' st ' sk=' sk.
    move 'two     ' to rels-rec. write rels-rec. display 'w ' st ' sk=' sk.
    move 'three   ' to rels-rec. write rels-rec. display 'w ' st ' sk=' sk.
    close rels.
    open i-o rels.
    read rels at end display 'end1' end-read. display 'r ' st ' sk=' sk ' [' rels-rec ']'.
    move 'ONE     ' to rels-rec. rewrite rels-rec. display 'rw ' st.
    read rels at end display 'end2' end-read. display 'r ' st ' sk=' sk ' [' rels-rec ']'.
    delete rels record. display 'del ' st.
    read rels at end display 'end3' end-read. display 'r ' st ' sk=' sk ' [' rels-rec ']'.
    read rels at end display 'end4 ' st end-read.
    rewrite rels-rec. display 'rw after end ' st.
    close rels.
    move 'n' to eof.
    open input rels.
    perform until eof = 'y'
        read rels at end move 'y' to eof
            not at end display 'n ' st ' sk=' sk ' [' rels-rec ']'
        end-read
    end-perform.
    close rels.
    stop run.
