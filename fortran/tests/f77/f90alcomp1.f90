module poly
  implicit none
  type polynomial
    integer :: deg
    real, allocatable :: c(:)
  end type
  type pair
    character(len=4) :: tag
    type(polynomial) :: p, q
  end type
  interface operator(+)
    module procedure padd
  end interface
contains
  function make(n, v) result(r)
    integer, intent(in) :: n
    real, intent(in) :: v
    type(polynomial) :: r
    integer :: i
    r%deg = n
    allocate(r%c(0:n))
    do i = 0, n
      r%c(i) = v + i
    end do
  end function
  function padd(a, b) result(r)
    type(polynomial), intent(in) :: a, b
    type(polynomial) :: r
    integer :: i
    r%deg = max(a%deg, b%deg)
    allocate(r%c(0:r%deg))
    r%c = 0.
    r%c(0:a%deg) = a%c
    do i = 0, b%deg
      r%c(i) = r%c(i) + b%c(i)
    end do
  end function
  real function eval(p, x)
    type(polynomial), intent(in) :: p
    real, intent(in) :: x
    integer :: i
    eval = 0.
    do i = p%deg, 0, -1
      eval = eval * x + p%c(i)
    end do
  end function
  subroutine reset(p)
    type(polynomial), intent(out) :: p
    print *, 'in reset:', allocated(p%c)
    p%deg = 1
    allocate(p%c(0:1))
    p%c = (/ 9., 8. /)
  end subroutine
  subroutine scratch(k)
    integer :: k
    type(polynomial) :: t
    print *, 'scratch entry:', allocated(t%c)
    allocate(t%c(k))
    t%c = 1.
    print *, sum(t%c)
  end subroutine
end module

program t
  use poly
  implicit none
  type(polynomial) :: a, b, c
  type(pair) :: pr
  integer :: i
  print *, allocated(a%c)
  a = make(2, 1.0)
  b = make(3, 0.5)
  print *, allocated(a%c), size(a%c), lbound(a%c), ubound(a%c)
  print *, a%c, eval(a, 2.0)
  c = a + b
  print *, c%deg, c%c
  c = a            ! deep copy
  a%c(0) = 100.
  print *, c%c, a%c
  c = c
  print *, c%c
  pr%tag = 'x'
  pr%p = a
  pr%q = b + b
  print *, allocated(pr%p%c), pr%p%c, pr%q%c
  call reset(c)
  print *, c%deg, c%c
  do i = 1, 3
    call scratch(i)
  end do
  deallocate(a%c)
  print *, allocated(a%c), allocated(b%c)
  a = make(1, 2.0)
  do i = 1, 20000
    c = a + b + b
  end do
  print *, eval(c, 1.0)
end program
