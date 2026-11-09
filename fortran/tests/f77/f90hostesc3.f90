! DOUBLE PRECISION and COMPLEX locals as operands of a defined operation
! and a defined assignment, as items of a READ's implied DO, and as a
! pointer's target.
module m2g
  implicit none
  interface operator(.plus.)
    module procedure dplus
  end interface
  interface assignment(=)
    module procedure asg
  end interface
  type box
    real(8) :: v
  end type
contains
  function dplus(a, b) result(r)
    real(8), intent(in) :: a
    complex, intent(in) :: b
    real(8) :: r
    r = a + real(b) + aimag(b)
  end function
  subroutine asg(b, x)
    type(box), intent(out) :: b
    real(8), intent(in) :: x
    b%v = x * 2
  end subroutine
  subroutine s(n)
    integer :: n, i
    real(8) :: d, my_d, e
    complex :: z
    real(8), pointer :: p
    real(8), target :: t1
    type(box) :: b
    d = 0.25d0 * n
    z = cmplx(n, 0.5)
    my_d = 1.0d0 / 8
    print *, d .plus. z
    b = d
    print *, b%v
    open (9, status='scratch')
    write (9, *) 2.5d0, 7.25d0
    rewind 9
    read (9, *) (e, my_d, i = 1, 1)
    close (9)
    print *, e, my_d
    t1 = 0.125d0 * n
    p => t1
    p = p + 1
    print *, t1
    t1 = t1 + 1
    print *, p
  end subroutine
end module
program p2g
  use m2g
  call s(1)
  call s(3)
end program
