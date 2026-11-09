! Module variables (Fortran 90 11.3): initialized and not, arrays,
! DOUBLE PRECISION, CHARACTER, LOGICAL, an ALLOCATABLE allocated in a
! module procedure and used, deallocated, in the program; a second module
! USEing the first with ONLY and changing its variables; a module
! procedure's local hiding one by host association; a rename in an
! internal procedure's USE.
module f90mvc
  implicit none
  integer :: ncall = 0
  integer :: hist(5)
  double precision :: total = 1.5d0
  character(len=8) :: tag = 'start'
  real, allocatable :: buf(:)
  integer, parameter :: nmax = 5
  logical :: ready
contains
  subroutine bump(k)
    integer, intent(in) :: k
    ncall = ncall + 1
    hist(mod(k, nmax) + 1) = hist(mod(k, nmax) + 1) + 1
    total = total + k
  end subroutine
  subroutine setup(n)
    integer :: n, i
    allocate(buf(n))
    buf = (/ (real(i), i = 1, n) /)
    ready = .true.
    tag = 'ready'
  end subroutine
  integer function shadow()
    integer :: ncall
    ncall = 99
    shadow = ncall
  end function
end module

module f90mvs
  use f90mvc, only: ncall, total
  implicit none
  integer :: extra = 7
contains
  subroutine twice()
    ncall = ncall + 2
    total = total * 2
    extra = extra + 1
  end subroutine
end module

program f90modvar1
  use f90mvc
  use f90mvs
  implicit none
  integer :: i
  hist = 0
  ready = .false.
  do i = 1, 12
    call bump(i)
  end do
  print *, ncall, hist, total, '[', tag, ']', ready
  call setup(4)
  print *, buf, sum(buf), size(buf), allocated(buf), tag, ready
  call twice()
  print *, ncall, total, extra, shadow(), ncall
  call user2()
  print *, ncall, buf
  deallocate(buf)
  print *, allocated(buf)
contains
  subroutine user2()
    use f90mvc, only: n => ncall, b => buf
    n = n + 100
    b(2) = -1.0
  end subroutine
end program
