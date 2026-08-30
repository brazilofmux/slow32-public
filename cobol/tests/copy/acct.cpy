*>fd  accounts copy 'acct.cpy'.  -- the tail of an FD, then its record,
*>  in the shape of majesty's src/copy/ books
    block contains 2000 records.
01  account-record.
    88  end-of-accounts     value high-values.
    05  act-key             packed-decimal pic 9(6).
    05  act-crdb            pic x.
    05  act-balance         pic 9(7)v99+.
    05  act-name            pic x(20).
