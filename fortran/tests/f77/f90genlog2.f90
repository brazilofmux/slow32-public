module m
  implicit none
  interface show
    module procedure show_i, show_l, show_ia, show_la
  end interface
  type t
    integer :: n
    logical :: f
    integer :: v(2)
    logical :: g(2)
  end type
  integer, parameter :: pi = 7
  logical, parameter :: pl = .true.
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
  integer function fi(l)
    logical, intent(in) :: l
    fi = 3
    if (l) fi = 4
  end function
  logical function fl(i)
    integer, intent(in) :: i
    fl = i > 2
  end function
  function fia(n)
    integer, intent(in) :: n
    integer :: fia(n), i
    do i = 1, n
      fia(i) = i * i
    end do
  end function
  function fla(n)
    integer, intent(in) :: n
    logical :: fla(n)
    integer :: i
    do i = 1, n
      fla(i) = mod(i, 2) == 0
    end do
  end function
end module
program p
  use m
  implicit none
  integer :: i, k(3)
  logical :: l, q(3)
  type(t) :: s, r(2)
  integer, pointer :: ip
  integer, target :: it
  integer, allocatable :: al(:)
  logical, allocatable :: ql(:)
  real :: x
  character(5) :: c
  i = 4; l = .true.; x = 2.5; c = 'hello'
  k = (/ 1, 2, 3 /); q = (/ .true., .false., .true. /)
  s = t(5, .false., (/ 8, 9 /), (/ .true., .false. /))
  r = s
  r(2)%f = .true.
  nullify(ip)
  allocate(al(2), ql(2)); al = 6; ql = .false.
  call show(merge(1, 2, l))
  call show(merge(l, .false., i > 3))
  call show(count(q))
  call show(len(c))
  call show(int(x))
  call show(k(1) + size(k))
  call show(ichar('a'))
  call show(fi(l))
  call show(fl(i))
  call show(fi(fl(i)))
  call show(fl(fi(l)))
  call show(s%n)
  call show(s%f)
  call show(s%v)
  call show(s%g)
  call show(s%v(2))
  call show(s%g(1))
  call show(r%n)
  call show(r%f)
  call show(r(2)%f)
  call show(r(2)%v)
  call show(.not. q(1))
  call show(any(q))
  call show(all(q))
  call show(l .eqv. l)
  call show(i == 1)
  call show(index(c, 'l'))
  call show(pi)
  call show(pl)
  call show(pi + 1)
  call show(.not. pl)
  call show(lge('a', 'b'))
  call show(btest(i, 2))
  call show(iand(i, 6))
  call show(max(i, 2))
  call show(-i)
  call show((l))
  call show((i))
  call show(maxval(k))
  call show(dot_product(q, q))
  call show(dot_product(k, k))
  call show(associated(ip))
  call show(allocated(al))
  call show(mod(i, 3))
  call show(huge(i))
  call show(kind(i))
  call show(kind(l))
  call show(size(q))
  call show(lbound(k, 1))
  call show(q .and. q)
  call show(.not. q)
  call show(merge(k, -k, q))
  call show(merge(q, .not. q, k > 1))
  call show(shape(k))
  call show((/ .true., .false. /))
  call show((/ 1, 2 /))
  call show(pack(k, q))
  call show(al)
  call show(ql)
  call show(al + 1)
  call show(ql .or. .true.)
  call show(fia(3))
  call show(fla(3))
  call show(fia(2) + 1)
  call show(.not. fla(2))
  call show(k(3:1:-1))
  call show(q(3:1:-1))
  call show(k(k))
  call show(maxloc(k))
  call show(nint(x))
  call show(x > 1.0)
  call show(c == 'hello')
  call show(c(1:1) < c(2:2))
  it = 3; ip => it
  call show(ip)
  call show(ip + 1)
  call show(associated(ip, it))
  call show(present_like())
contains
  logical function present_like()
    present_like = .false.
  end function
end program
