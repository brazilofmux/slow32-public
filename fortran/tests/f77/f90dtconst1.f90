module m
  implicit none
  integer, parameter :: dp = kind(1.0d0), k = 3
  integer, parameter :: dims(2) = (/ 2, 4 /)
  type t
     real(kind=dp) :: x
     real(dp) :: y(k)
     character(len=k) :: c
     integer :: k
     integer :: g(dims(1), dims(2))
     real :: h(max(k, 2) + 1)
  end type t
contains
  subroutine show(a)
    type(t), intent(in) :: a
    type loc
       real(dp) :: q(k)
       character(k + 1) :: s
    end type loc
    type(loc) :: l
    l%q = a%y * 2
    l%s = a%c // '!'
    print *, a%x, a%y, a%c, a%k, size(a%g, 1), size(a%g, 2), size(a%h)
    print *, l%q, l%s, kind(l%q), len(l%s)
  end subroutine show
end module m
program p
  use m
  implicit none
  integer, parameter :: j = 2
  type u
     real(dp) :: x
     character(j) :: c
     type(t) :: tt
     integer :: j(j)
  end type u
  type(u) :: v
  v%x = 1.5d0
  v%c = 'ab'
  v%tt%x = 2.5d0
  v%tt%y = (/ 1d0, 2d0, 3d0 /)
  v%tt%c = 'xyz'
  v%tt%k = 9
  v%j = (/ 7, 8 /)
  call show(v%tt)
  print *, v%x, v%c, v%j, kind(v%x), len(v%c), size(v%j)
  call inner
contains
  subroutine inner
    type w
       integer :: a(j, k)
    end type w
    type(w) :: z
    z%a = 1
    print *, size(z%a), shape(z%a)
  end subroutine inner
end program p
