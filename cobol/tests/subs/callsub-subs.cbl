       identification division.
       program-id. addup.
       data division.
       linkage section.
       01  x       pic s9(4) comp.
       01  y       pic s9(4) comp.
       01  z       pic s9(6) comp.
       procedure division using x y z.
           add x y giving z.
           goback.
       end program addup.
       identification division.
       program-id. counter.
      * WORKING-STORAGE of a called program is static: calls accumulate
       data division.
       working-storage section.
       01  calls   pic 99 value 0.
       linkage section.
       01  out     pic 99.
       procedure division using out.
           add 1 to calls.
           move calls to out.
           goback.
       end program counter.
       identification division.
       program-id. greet.
       data division.
       linkage section.
       01  who.
           05  w-name  pic x(10).
           05  w-age   pic 99.
       01  line-out pic x(12).
       procedure division using who line-out.
           move spaces to line-out.
           string 'hi ' delimited by size w-name delimited by space
               into line-out.
           add 1 to w-age.
           goback.
       end program greet.
       identification division.
       program-id. twoup.
       data division.
       working-storage section.
       01  two     pic s9(4) comp value 2.
       linkage section.
       01  v       pic s9(4) comp.
       01  r       pic s9(6) comp.
       procedure division using v r.
           call 'addup' using v two r.
           add v to r.
           goback.
       end program twoup.
       identification division.
       program-id. early.
       data division.
       linkage section.
       01  v       pic s9(4) comp.
       procedure division using v.
           add 1 to v.
           if v > 100 exit program end-if.
           add 1000 to v.
           goback.
       end program early.
