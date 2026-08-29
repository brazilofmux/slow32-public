       identification division.
       program-id. cbridge.
      * COBOL calling COBOL calling C, on one convention -- the SLOW-32
      * C ABI.  BY VALUE / RETURNING are 2002 words: default dialect.
       data division.
       working-storage section.
       01  ld              usage signed-int value 155468.
       01  result.
           05  fielded.
               10 year        sync usage signed-short.
               10 month       sync usage unsigned-short.
               10 dayofweek   sync usage unsigned-short.
               10 dayofmonth  sync usage unsigned-short.
               10 dayofyear   sync usage unsigned-short.
           05  ok             pic x.
       01  y               pic s9(4) value 2026.
       01  m               pic 99 value 8.
       01  d               pic 99 value 29.
       01  packed-out.
           05  value-out      usage signed-int.
           05  ok             pic x.
       01  n               usage signed-int value 42.
       01  r               usage signed-int.
       procedure division.
           call 'c_fill' using ld result.
           display ok of result ' ' year ' ' month ' ' dayofweek ' '
                   dayofmonth ' ' dayofyear.
           move -1 to ld.
           call 'c_fill' using ld result.
           display ok of result.
           call 'c_pack' using y m d packed-out.
           display ok of packed-out ' ' value-out.
           move 13 to m.
           call 'c_pack' using y m d packed-out.
           display ok of packed-out.
           call 'bt_neg' using by value n returning r.
           display r.
           call 'bt_neg' using by value 7 returning r.
           display r.
           stop run.
       end program cbridge.
