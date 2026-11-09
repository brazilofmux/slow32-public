! Array-valued functions (Fortran 90): internal and module, results of
! constant shape, automatic (from a dummy, from SIZE of an assumed-shape
! dummy) and with lower bounds other than 1; INTEGER, DOUBLE PRECISION,
! LOGICAL, COMPLEX and CHARACTER; as an assignment's right side, twice in
! one expression, in reductions, TRANSPOSE, a WHERE mask, a PRINT list,
! concatenation, as the actual of an assumed-shape dummy and of another
! array-valued function; a RETURN before the end.  Each result is
! copied out at the return into storage the caller frees after the
! statement (it had been taken for a scalar and stored to every element).
module f90afm
  implicit none
contains
  function ramp(n)
    integer, intent(in) :: n
    double precision :: ramp(n)
    integer :: i
    do i = 1, n
      ramp(i) = 0.5d0 * i
    end do
  end function
  function flags(x)
    real, intent(in) :: x(:)
    logical :: flags(size(x))
    flags = x > 0.0
  end function
end module

program f90afunc1
  use f90afm
  implicit none
  integer :: v(4), m(2,3), i
  real :: a(5)
  double precision :: d(3)
  complex :: z(2)
  character(len=3) :: c(3)
  v = sq(4)
  print *, v
  v = 2 * sq(4) + sq(4)
  print *, v
  print *, sum(sq(10)), maxval(sq(3)), sq(3)
  m = grid(2, 3)
  print *, m
  print *, transpose(grid(2, 3))
  d = ramp(3)
  print *, d, sum(ramp(5))
  a = (/ 1.0, -2.0, 3.0, -4.0, 5.0 /)
  print *, flags(a), count(flags(a(2:5)))
  where (flags(a)) a = 0.0
  print *, a
  z = cz()
  print *, z
  c = tags(3)
  print *, c, tags(2) // '!'
  call show(sq(3))
  v(2:4) = sq(3) + 1
  print *, v
  print *, edge(5)
contains
  function sq(n)
    integer :: n, sq(n), i
    do i = 1, n
      sq(i) = i * i
    end do
  end function
  function grid(p, q)
    integer :: p, q
    integer :: grid(p, q)
    integer :: i, j
    do j = 1, q
      do i = 1, p
        grid(i, j) = 10 * i + j
      end do
    end do
  end function
  function cz()
    complex :: cz(2)
    cz(1) = (1.0, 2.0)
    cz(2) = conjg(cz(1)) * 2.0
  end function
  function tags(n)
    integer :: n
    character(len=3) :: tags(n)
    integer :: i
    do i = 1, n
      tags(i) = 'T' // char(48 + i) // 'x'
    end do
  end function
  subroutine show(k)
    integer :: k(:)
    print *, 'show', size(k), k
  end subroutine
  function edge(n)
    integer :: n
    integer :: edge(0:n-1)
    integer :: i
    edge = -1
    do i = 0, n - 1
      if (i == 3) return
      edge(i) = i
    end do
  end function
end program
