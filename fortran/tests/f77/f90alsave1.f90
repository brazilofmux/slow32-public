module m
  type t
     integer :: n
     real, allocatable :: v(:)
  end type t
end module m
subroutine h(k)
  use m
  integer :: k
  integer, allocatable, save :: a(:)
  integer, allocatable :: b(:)
  type(t), save :: s
  type(t) :: u
  real, save :: acc
  data acc /0.0/
  integer :: cnt = 0
  if (.not. allocated(a)) then
     allocate(a(3)); a = k
     print *, 'fresh a'
  end if
  if (.not. allocated(b)) then
     allocate(b(2)); b = k
     print *, 'fresh b'
  end if
  if (.not. allocated(s%v)) then
     allocate(s%v(2)); s%v = k; s%n = 0
     print *, 'fresh s'
  end if
  if (.not. allocated(u%v)) then
     allocate(u%v(2)); u%v = k
     print *, 'fresh u'
  end if
  call inner
  print *, a, b, s%n, s%v, u%v, acc, cnt
contains
  subroutine inner
    a(1) = a(1) + 1
    b(1) = b(1) + 1
    s%n = s%n + 1
    s%v(2) = s%v(2) + 1
    u%v(1) = u%v(1) + 1
    acc = acc + 0.5
    cnt = cnt + 1
  end subroutine inner
end subroutine h
subroutine g(k)
  integer :: k
  save
  integer, allocatable :: a(:)
  integer :: j = 0
  if (.not. allocated(a)) then
     allocate(a(2)); a = k
     print *, 'fresh g'
  end if
  call inner
  print *, a, j
contains
  subroutine inner
    a(2) = a(2) + 1
    j = j + 1
  end subroutine inner
end subroutine g
program p
  call h(1); call h(10); call h(100)
  call g(1); call g(10)
end program p
