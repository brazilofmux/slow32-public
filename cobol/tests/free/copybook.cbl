*> COPY, the Library module: an FD whose clauses and record come from a
*> copybook (majesty's `fd x copy sbook.` shape), WORKING-STORAGE items
*> from another, and procedure text from a third; the copybooks are
*> found through -I.  COMP-3 amounts: default dialect.
identification division.
program-id. copybook.
environment division.
input-output section.
file-control.
    select accounts assign to 'tmp/accounts.dat'
        organization is sequential.
data division.
file section.
fd  accounts copy 'acct.cpy'.
working-storage section.
copy wsbook.
01  i           pic 9 comp.
procedure division.
    open output accounts.
    perform varying i from 1 by 1 until i > 3
        move i to act-key
        move 'D' to act-crdb
        compute act-balance = i * 1000.25
        move 'account' to act-name
        write account-record
    end-perform.
    close accounts.
    open input accounts.
    perform until at-eof
        read accounts
            at end move 'Y' to ws-eof
            not at end
                add 1 to ws-count
                move act-balance to ws-amt
                add ws-amt to ws-total
                display act-key ' ' act-crdb ' [' act-balance '] ' act-name
        end-read
    end-perform.
    close accounts.
    copy para.
    stop run.
