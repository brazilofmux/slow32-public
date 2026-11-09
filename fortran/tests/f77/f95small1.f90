! Fortran 95's smaller additions: KIND on CEILING, FLOOR and NINT, SIGN of a
! negative zero, I0/F0.d/B0/O0/Z0, CPU_TIME, MAXLOC and MINLOC with DIM, a
! PURE function in a specification expression, a comment in NAMELIST input,
! END INTERFACE with its generic.
module f95small1_m
contains
  pure function len_n(n) result(k)
    integer, intent(in) :: n
    integer :: k
    k = n + 1
  end function
  function f(n) result(r)
    integer :: n
    integer :: r(len_n(n))
    r = n
  end function
end module f95small1_m
program f95small1
  use f95small1_m
  implicit none
  integer :: m(2,3), i
  real :: t1
  namelist /nl/ i, t1
  m = reshape((/ 1, 9, 3, 4, 8, 6 /), (/ 2, 3 /))
  print *, maxloc(m, dim=1), minloc(m, dim=2)
  print *, maxloc(m, mask=m < 8)
  print *, ceiling(2.5, kind=4), floor(-2.5, 4)
  print *, sign(1.0, -0.0)
  write (*, '(I0,1X,I0,1X,F0.2,1X,I0.3)') 42, -7, 3.14159, 5
  write (*, '(B0,1X,O0,1X,Z0)') 5, 64, 255
  call cpu_time(t1)
  print *, t1 >= 0.0
  i = 3
  print *, f(i)
  open (9, status='scratch')
  write (9, '(A)') '&nl i = 5 ! a comment', ' t1 = 2.5 /'
  rewind 9
  read (9, nl)
  print *, i, t1
end program
