module ma
  implicit none
  type inner
    integer :: a = 3
    double precision :: d = 2.5d0
  end type
  type t
    integer :: n = 7
    real :: w(3) = (/ 1.5, 2.5, 3.5 /)
    character(5) :: c = 'abc'
    type(inner) :: in
    integer, pointer :: p => null()
    integer :: tail(40) = -1
  end type
  type z
    integer :: k = 0
    real :: r = 0.0
  end type
  type plain
    integer :: k
  end type
  type(t) :: mv
  type(t), save :: marr(3)
contains
  subroutine bump
    mv%n = mv%n + 1
    marr(3)%in%a = marr(3)%in%a + 10
  end subroutine
end module
