! Four FORALLs gfortran (14.2 and 16.2) does not take as Fortran 95 has
! them, so f95forall10.expected pins the output rather than gfortran:
!
! - an index's values are all read before any is stored (7.5.4.2.3), also
!   when what is read is the stored array under another name -- through
!   EQUIVALENCE, or inside a PURE function that has it by host or use
!   association.  gfortran stores as it goes and prints 1 1 1 1 1 1 and
!   1 10 100 1000 10000 100000;
! - a FORALL statement ends a labelled DO: it is an action statement, and
!   not among those 8.1.4.1.1 keeps from ending one.  gfortran refuses it;
! - a FORALL in a FORALL, its bounds naming the outer index, that has a
!   mask or reads what it stores.  gfortran sizes what it sets aside from
!   the index before the index has a value (-Wall: "'i' is used
!   uninitialized"; -fsanitize=address: a one-byte mask written past), and
!   14.2 dies of it when other FORALLs have run first.
module f95forall10m
  implicit none
  integer :: g(6)
contains
  pure integer function mg(i)
    integer, intent(in) :: i
    mg = g(i) * 10
  end function
end module
program f95forall10
  use f95forall10m
  implicit none
  integer :: a(6), e1(6), e2(6), v(9), m(4, 4), i, j, k
  real :: r(6)
  equivalence (e1, e2)
  e1 = (/ 1, 2, 3, 4, 5, 6 /)
  forall (i = 2:6) e1(i) = e2(i - 1)
  print *, e1
  a = (/ (i, i = 1, 6) /)
  forall (i = 2:6) a(i) = f(i - 1)
  print *, a
  g = (/ (i, i = 1, 6) /)
  forall (i = 2:6) g(i) = mg(i - 1)
  print *, g
  r = (/ (real(i), i = 1, 6) /)
  forall (i = 2:6) r(i) = sqrt(r(i - 1)) + h(i - 1)
  print '(6f9.4)', r
  forall (i = 1:5, f(i + 1) > 30) a(i) = f(i + 1)
  print *, a
  v = (/ (i, i = 1, 9) /)
  do 10 k = 1, 3
10 forall (i = 1:9, v(i) > k) v(i) = v(i) - k
  print *, v
  m = 0
  forall (i = 1:4)
    forall (j = i:4) m(i, j) = i + j
  end forall
  forall (i = 1:4)
    forall (j = 1:i - 1, m(j, i) > 4) m(i, j) = -m(j, i)
  end forall
  print *, m
  forall (i = 1:4)
    forall (j = 1:i - 1) m(i, j) = m(j, i) * 10
  end forall
  print *, m
contains
  pure integer function f(i)
    integer, intent(in) :: i
    f = a(i) * 10
  end function
  pure real function h(i)
    integer, intent(in) :: i
    h = r(i)
  end function
end program
