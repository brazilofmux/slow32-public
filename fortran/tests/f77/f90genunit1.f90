! Generic interfaces outside a module (Fortran 90 12.3.2.1): a main
! program's, of interface bodies for externals of this file -- a
! FUNCTION generic and a SUBROUTINE one -- reached from its internal
! procedure too.
program f90genunit1
  implicit none
  interface f90gutwice
    real function f90gutwr(x)
      real :: x
    end function
    integer function f90gutwi(i)
      integer :: i
    end function
  end interface
  interface dbl
    subroutine f90gutws(c)
      character(len=*) :: c
    end subroutine
  end interface
  character(len=6) :: c
  c = 'ab'
  print *, f90gutwice(1.5), f90gutwice(4)
  call dbl(c)
  print *, c
  call inner()
contains
  subroutine inner()
    print *, 'inner', f90gutwice(10)
  end subroutine
end program
real function f90gutwr(x)
  real :: x
  f90gutwr = 2 * x
end function
integer function f90gutwi(i)
  integer :: i
  f90gutwi = 2 * i
end function
subroutine f90gutws(c)
  character(len=*) :: c
  c = trim(c) // trim(c)
end subroutine
