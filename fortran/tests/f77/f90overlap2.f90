! The value of an array assignment reaches its target through a
! structure's component, through a function that reads the target, or as
! a substring of it: evaluated whole first (F90 7.5.1.5).  (A pointer
! component against its target by name is f90overlap6, pinned.)
module gm
  implicit none
  integer :: ga(6)
contains
  integer function g1()
    g1 = ga(1)
  end function
  elemental integer function ge(i)
    integer, intent(in) :: i
    ge = ga(i)
  end function
end module
program f90overlap2
  use gm
  implicit none
  type t
    integer :: a(6)
    integer, pointer :: p(:)
  end type
  type(t) :: x
  type(t) :: xs(2)
  integer, target :: a(6)
  integer :: i
  character(4) :: c(4)
  x%a = (/ (i, i = 1, 6) /)
  x%a(2:6) = x%a(1:5)
  print *, x%a
  x%a = (/ (i, i = 1, 6) /)
  x%a(1:5) = x%a(2:6) + x%a(6:2:-1)
  print *, x%a
  x%a = (/ (i, i = 1, 6) /)
  x%a = x%a(6:1:-1)
  print *, x%a
  xs(1)%a = (/ (i, i = 1, 6) /)
  xs(2)%a = 0
  xs(1)%a(2:6) = xs(1)%a(1:5)
  print *, xs(1)%a
  a = (/ (i, i = 1, 6) /)
  x%p => a
  x%p(2:6) = x%p(1:5)
  print *, a
  ga = (/ (i, i = 1, 6) /)
  ga(1:5) = ga(2:6) + g1()
  print *, ga
  ga = (/ (i, i = 1, 6) /)
  ga(2:6) = ge((/ 1, 2, 3, 4, 5 /))
  print *, ga
  ga = (/ (i, i = 1, 6) /)
  a = (/ (i, i = 1, 6) /)
  ga(1:5) = a(1:5) + g1()
  print *, ga
  a = (/ (i, i = 1, 6) /)
  print *, a
  c = (/ 'abcd', 'efgh', 'ijkl', 'mnop' /)
  c(2:4) = c(1:3)
  print *, c
  c = (/ 'abcd', 'efgh', 'ijkl', 'mnop' /)
  c(2:4)(2:3) = c(1:3)(1:2)
  print *, c
  c = (/ 'abcd', 'efgh', 'ijkl', 'mnop' /)
  c(1)(2:4) = c(1)(1:3)
  print *, c
end program
