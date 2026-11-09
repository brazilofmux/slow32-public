! RECURSIVE and RESULT (Fortran 90 12.5.2.2): recursive functions, module
! and internal, of INTEGER, DOUBLE PRECISION, LOGICAL, CHARACTER and
! array results (REV calls itself through the hoist); typed by a prefix
! or by the result's declaration; mutual recursion; recursive
! subroutines (quicksort, Hanoi); RESULT without recursion; a contained
! procedure's own IMPLICIT giving its letters back from the host's
! IMPLICIT NONE.
module f90rcm
  implicit none
contains
  recursive function fib(n) result(f)
    integer, intent(in) :: n
    integer :: f
    if (n < 2) then
      f = n
    else
      f = fib(n - 1) + fib(n - 2)
    end if
  end function
  recursive function rev(v) result(r)
    integer, intent(in) :: v(:)
    integer :: r(size(v))
    if (size(v) <= 1) then
      r = v
    else
      r(1) = v(size(v))
      r(2:) = rev(v(1:size(v) - 1))
    end if
  end function
end module
program f90recur1
  use f90rcm
  implicit none
  integer :: a(8), i
  double precision :: d
  character(len=12) :: s
  print *, fib(15), ack(2, 3)
  print *, rev((/ 1, 2, 3, 4, 5 /))
  a = (/ 5, 3, 8, 1, 9, 2, 7, 4 /)
  call qsort(a, 1, 8)
  print *, a
  d = dfact(10)
  print *, d
  s = stars(5)
  print *, '[', s, ']', isodd(7), iseven(7)
  print *, plain(4), implicit_i(3.7)
  call hanoi(3, 1, 3, 2)
contains
  recursive integer function ack(m, n) result(r)
    integer :: m, n
    if (m == 0) then
      r = n + 1
    else if (n == 0) then
      r = ack(m - 1, 1)
    else
      r = ack(m - 1, ack(m, n - 1))
    end if
  end function
  recursive subroutine qsort(x, lo, hi)
    integer :: x(:), lo, hi, p, i, j, t
    if (lo >= hi) return
    p = x((lo + hi) / 2)
    i = lo
    j = hi
    do while (i <= j)
      do while (x(i) < p)
        i = i + 1
      end do
      do while (x(j) > p)
        j = j - 1
      end do
      if (i <= j) then
        t = x(i); x(i) = x(j); x(j) = t
        i = i + 1; j = j - 1
      end if
    end do
    call qsort(x, lo, j)
    call qsort(x, i, hi)
  end subroutine
  recursive function dfact(n) result(r)
    integer :: n
    double precision :: r
    if (n <= 1) then
      r = 1d0
    else
      r = n * dfact(n - 1)
    end if
  end function
  recursive function stars(n) result(r)
    integer :: n
    character(len=12) :: r
    if (n == 0) then
      r = ''
    else
      r = '*' // stars(n - 1)
    end if
  end function
  recursive logical function isodd(n) result(b)
    integer :: n
    if (n == 0) then
      b = .false.
    else
      b = iseven(n - 1)
    end if
  end function
  recursive logical function iseven(n) result(b)
    integer :: n
    if (n == 0) then
      b = .true.
    else
      b = isodd(n - 1)
    end if
  end function
  function plain(n) result(sq)
    integer :: n, sq
    sq = n * n
  end function
  function implicit_i(x) result(ires)
    implicit integer (i)
    real :: x
    ires = int(x)
  end function
  recursive subroutine hanoi(n, from, to, via)
    integer :: n, from, to, via
    if (n == 0) return
    call hanoi(n - 1, from, via, to)
    print *, 'move', n, from, to
    call hanoi(n - 1, via, to, from)
  end subroutine
end program
