! Common blocks across files: this one and files2.f90.  The other file names /shared/
! and blank common at sizes of its own, as the standard lets blank common be, and
! holds the BLOCK DATA that gives /preset/ its values.
program files
  implicit none
  integer :: left(3), right, here(2), preset(4), after
  common /shared/ left, right
  common here
  common /preset/ preset
  common /next/ after
  external filldata
  call fill
  if (any(left /= [31, 32, 33]) .or. right /= 34) stop 1
  if (any(here /= [41, 42])) stop 2
  if (any(preset /= [51, 52, 53, 54])) stop 3
  if (after /= 0) stop 4
  left(2) = 61
  call check
  print '(a)', 'files-ok'
end program
