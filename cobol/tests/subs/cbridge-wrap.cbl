       identification division.
       program-id. c_fill.
      * clinkages.cbl's shape: a COBOL program wrapping a C entry point,
      * BY VALUE / BY REFERENCE / RETURNING only here at the seam
       data division.
       working-storage section.
       01  isvalid            usage signed-int.
           88  notvalid value 0.
       linkage section.
       01  ld                 usage signed-int.
       01  result.
           05  fielded.
               10 year        sync usage signed-short.
               10 month       sync usage unsigned-short.
               10 dayofweek   sync usage unsigned-short.
               10 dayofmonth  sync usage unsigned-short.
               10 dayofyear   sync usage unsigned-short.
           05  ok             pic x.
               88  is-valid   value 'Y'.
               88  is-not-valid value 'N'.
       procedure division using ld result.
           call 'bt_fill' using by value ld by reference fielded
               returning isvalid.
           if not notvalid move 'Y' to ok else move 'N' to ok end-if.
           goback.
       end program c_fill.
       identification division.
       program-id. c_pack.
       data division.
       working-storage section.
       01  isvalid            usage signed-int.
           88  notvalid value 0.
       01  yy                 usage signed-short.
       01  mm                 usage unsigned-short.
       01  dd                 usage unsigned-short.
       linkage section.
       01  in-year            pic s9(4).
       01  in-month           pic 99.
       01  in-day             pic 99.
       01  packed-out.
           05  value-out      usage signed-int.
           05  ok             pic x.
       procedure division using in-year in-month in-day packed-out.
           move in-year to yy.
           move in-month to mm.
           move in-day to dd.
           call 'bt_pack' using by value yy by value mm by value dd
                by reference value-out returning isvalid.
           if notvalid move 'N' to ok else move 'Y' to ok end-if.
           goback.
       end program c_pack.
