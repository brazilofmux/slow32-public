module m
  implicit none
  interface show
    module procedure show_i, show_l, show_ia, show_la
  end interface
  interface operator(.x.)
    module procedure xi, xl
  end interface
  interface assignment(=)
    module procedure asg_tl, asg_ti
  end interface
  type t
    integer :: n
  end type
contains
  subroutine show_i(k)
    integer, intent(in) :: k
    print *, 'integer', k
  end subroutine
  subroutine show_l(k)
    logical, intent(in) :: k
    print *, 'logical', k
  end subroutine
  subroutine show_ia(k)
    integer, intent(in) :: k(:)
    print *, 'integers', k
  end subroutine
  subroutine show_la(k)
    logical, intent(in) :: k(:)
    print *, 'logicals', k
  end subroutine
  elemental integer function xi(a, b)
    integer, intent(in) :: a, b
    xi = a * 10 + b
  end function
  elemental logical function xl(a, b)
    logical, intent(in) :: a, b
    xl = a .neqv. b
  end function
  elemental subroutine asg_tl(a, b)
    type(t), intent(out) :: a
    logical, intent(in) :: b
    a%n = -1
    if (b) a%n = 1
  end subroutine
  elemental subroutine asg_ti(a, b)
    type(t), intent(out) :: a
    integer, intent(in) :: b
    a%n = b * 100
  end subroutine
end module
program p
  use m
  implicit none
  integer :: i, k(3)
  logical :: l, q(3)
  type(t) :: s, r(3)
  i = 4; l = .true.
  k = (/ 1, 2, 3 /); q = (/ .true., .false., .true. /)
  call show(i)
  call show(l)
  call show(i + 1)
  call show(i > 3)
  call show(.not. l)
  call show(k)
  call show(q)
  call show(k(2:))
  call show(q(2:))
  call show(k(2))
  call show(q(2))
  call show(k > 1)
  call show(k + 1)
  call show(7)
  call show(.false.)
  print *, i .x. 2, l .x. .false., l .x. l
  print *, k .x. i
  print *, q .x. l
  print *, (i > 2) .x. (i > 9)
  s = l;  print *, s%n
  s = i;  print *, s%n
  s = i < 2;  print *, s%n
  r = q;  print *, r%n
  r = k;  print *, r%n
  r = k > 2;  print *, r%n
  r = 5;  print *, r%n
  r = .true.;  print *, r%n
end program
