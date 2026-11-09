! An array constructor in a WHERE: evaluated whole, the mask choosing
! among its elements.
program p5
  implicit none
  integer :: a(6), b(6), i
  a = (/ 3, -1, 4, -1, 5, -9 /)
  b = 0
  where (a > 0) b = (/ (10*i, i = 1, 6) /)
  print *, b
  where (a < 0)
    b = (/ 1, 2, 3, 4, 5, 6 /) + a
  elsewhere
    b = b - (/ 6, 5, 4, 3, 2, 1 /)
  end where
  print *, b
  where ((/ .true., .false., .true., .false., .true., .false. /)) a = 0
  print *, a
end program p5
