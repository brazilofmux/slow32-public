! An ENTRY whose type is not its function's: each returns its own type,
! and the function keeps its own -- the entry typed by a declaration, by
! its RESULT, and beside a function that has a RESULT.
module mixed_a
  implicit none
contains
function count_of(n)
  implicit none
  integer :: n, count_of
  real :: share_of
  count_of = n + 100
  return
entry share_of(n)
  share_of = n / 8.0
  return
end function count_of
end module mixed_a
module mixed_b
  implicit none
contains
function tens(n)
  implicit none
  integer :: n, tens
  real :: frac
  tens = n * 10
  return
entry tenths(n) result(frac)
  frac = n * 0.1
  return
end function tens
function whole(n) result(w)
  implicit none
  integer :: n, w
  real :: part
  w = n - 1
  return
entry part(n)
  part = n + 0.25
  return
end function whole
end module mixed_b
program entrytype
  use mixed_a
  use mixed_b
  print *, count_of(6), share_of(6)
  print *, tens(6), tenths(6)
  print *, whole(6), part(6)
end
