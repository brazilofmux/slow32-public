! A whole LOGICAL array in list-directed output is T and F, as its
! elements are: it was written as INTEGERs, 1 and 0 (found 2026-11-08).
program f90logarr
logical l(2), m(2,2)
l = .true.
m = .false.
m(1,2) = .true.
print *, l
print *, m
print *, l(1), m(1,2)
end
